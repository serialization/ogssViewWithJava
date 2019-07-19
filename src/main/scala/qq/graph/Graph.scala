package qq.graph

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import ogss.common.java.api;
import qq.util.Vector

/** a graph showing objects from file in page */
class Graph(
    val file: qq.editor.File,
    val viewer: qq.editor.objects.ObjectGraph[_],
    val preferences: LayoutPreferences) {

  /**
   * nodes in the graph (indexed by the thing they represent).
   *  edges are found inside the nodes
   */
  val nodes: HashMap[AbstractNode, Node] = HashMap()

  def addNode(x: AbstractNode): Unit = {
    if (!nodes.contains(x)) {
      val n = new Node(this, x)
      nodes(x) = n
    }
  }
  /**
   * add edge x to the graph; add nodes for end points if necessary, append to existing edge is posible. If nodes
   *  are added, initialise their positions
   */
  def addEdge(x: AbstractEdge): Unit = {
    val f = x.getFrom
    val t = x.getTo
    var addedF = false
    var addedT = false
    val F = if (nodes.contains(f)) nodes(f) else {
      addedF = true;
      val F = new Node(this, f);
      nodes(f) = F;
      F
    }
    val T = if (nodes.contains(t)) nodes(t) else {
      addedT = true;
      val T = new Node(this, t);
      nodes(t) = T;
      T
    }
    if (addedF && !addedT) {
      F.pos = T.pos - x.idealDirection(file) * preferences.c2()
    } else if (addedT) {
      T.pos = F.pos + x.idealDirection(file) * preferences.c2()
    }
    // add to existing edge if one exists
    if (F.edgesOut.contains(T)) {
      F.edgesOut(T).data += x
    } else if (T.edgesOut.contains(F)) {
      T.edgesOut(F).reverseData += x
    } else {
      val X = new Edge(this, F, T, x)
      F.edgesOut(T) = X
      T.edgesIn(F) = X
    }
  }

  def energy: Float = nodes.values.map(_.energy).sum
  def energyHu: Float = nodes.values.map(x ⇒ x.force * x.force).sum

  def resetAccumulators: Unit = nodes.values.foreach(_.resetAccumulators)
  private def calculateForce(overlapAvoidance: Float, size: java.awt.Dimension): Unit = {
    val nvis = nodes.values.toIndexedSeq
    for (i ← 0 until nvis.size) {
      for (j ← i until nvis.size) {
        nvis(i).calculateForce(nvis(j), overlapAvoidance, size)
      }
      for (e ← nvis(i).edgesOut.values) {
        e.calculateForce()
      }
    }
  }
  def move(stepSize: Float): Unit = {
    nodes.values.foreach(_.move(stepSize))
  }

  val graphInfo = new GraphInfo

  /**
   * place nodes in a rectangle of the given size
   */
  def placeNodes(size: java.awt.Dimension): Unit = {
    /* centre and scale current placement *down* to fit into size if necessary */
    val xmin = nodes.values.map(_.pos.x).min
    val xmax = nodes.values.map(_.pos.x).max
    val ymin = nodes.values.map(_.pos.y).min
    val ymax = nodes.values.map(_.pos.y).max
    if (xmin < 0 || ymin < 0 || xmax > size.width || ymax > size.height) {
      val x0 = (xmin + xmax) / 2
      val y0 = (ymin + ymax) / 2
      val ax = if (xmax - xmin > size.width) size.width.toFloat / (xmax - xmin) else 1.0f
      val ay = if (ymax - ymin > size.height) size.height.toFloat / (ymax - ymin) else 1.0f
      nodes.values.foreach { x ⇒
        x.pos = new Vector(
          ax * (x.pos.x - x0) + size.width / 2,
          ay * (x.pos.y - y0) + size.height / 2)
      }
    }
    graphInfo.energyOfStep.clear()
    graphInfo.energyHuOfStep.clear()
    graphInfo.stepOfStep.clear()
    graphInfo.saveDirections(this)
    var stepsize: Float = 10
    var energyPreviousStep = Float.PositiveInfinity
    var stepsWithProgress = 0

    val initialIterations = preferences.initialIterations()
    val phaseInIterations = preferences.phaseInIterations()
    for (step ← 0.until(preferences.iterations)) {
      resetAccumulators
      calculateForce((((step - initialIterations + 9) / 10 * 10).toFloat / phaseInIterations).max(0).min(1), size)
      move(stepsize)
      graphInfo.energyOfStep += energy
      graphInfo.energyHuOfStep += energyHu
      graphInfo.stepOfStep += stepsize
      if (energyHu <= energyPreviousStep) {
        stepsWithProgress += 1
        if (stepsWithProgress >= 5) {
          stepsize /= 0.95f
          stepsWithProgress = 0
        }
      } else {
        stepsize *= 0.95f
        stepsWithProgress = 0
      }
      energyPreviousStep = energyHu
    }
    if (!cluttered) {
      updateIdealEdgeDirections
    }
    graphInfo.saveEnergy(energy, energyHu)
  }
  def cluttered: Boolean = energy / nodes.size > preferences.cluttered()
  private def updateIdealEdgeDirections = {
    val drawnEdges = for (n ← nodes.values; e ← n.edgesOut.values) yield e
    val abstractEdgesDirections = drawnEdges.toSeq.flatMap(e ⇒ e.data.iterator.map(x ⇒ (x, e.r)) ++ e.reverseData.iterator.map(x ⇒ (x, -e.r)))
    val dirByAbsEdge = abstractEdgesDirections
      .filter(_._1.isInstanceOf[SkillFieldEdge[_]])
      .groupBy(_._1.asInstanceOf[SkillFieldEdge[_]].field)
      .mapValues(x ⇒ Vector.avg(x.map(_._2.norm)))
    for ((f, x) ← dirByAbsEdge if (x.isFinite())) {
      val d = file.fieldPreferences(f).prefEdgeDirection
      val d0 = d()
      if (!file.fieldPreferences(f).prefFixedEdgeDirection()) {
        val c = preferences.c5().min(d0.abs)
        val d1 = d0 * c + x * (1 - c)
        d := d1.min(0.99f)
      }
    }
  }

  // info about the parameters of the layout, for comment in psexport
  class GraphInfo() {
    val edgeDirections = new HashMap[api.FieldDeclaration[_], Vector]
    var energy: Float = 0
    var energyHu: Float = 0
    val energyOfStep = new ArrayBuffer[Float]()
    val energyHuOfStep = new ArrayBuffer[Float]()
    val stepOfStep = new ArrayBuffer[Float]()

    def saveDirections(g: Graph) = {
      edgeDirections.clear()
      val fields = (for (
        v ← g.nodes.values;
        e ← v.edgesOut.values;
        e2 ← e.data ++ e.reverseData if e2.isInstanceOf[SkillFieldEdge[_]]
      ) yield e2.asInstanceOf[SkillFieldEdge[_]].field).toSet
      edgeDirections ++= fields.map(x ⇒ (x -> g.file.fieldPreferences(x).prefEdgeDirection()))
    }
    def saveEnergy(e: Float, e2: Float) = {
      energy = e
      energyHu = e2
    }
    def toPs: String = {
      s"% energy = $energy hu's energy: $energyHu\n"
      "% edge directions\n" +
        edgeDirections.map(x ⇒ s"% ${x._1.name}  ${x._2}\n").mkString("")

    }
  }
  // not nice, but better then screenshots
  def toPs(size: java.awt.Dimension): String = {
    "%!PS-Adobe-3.0 EPSF-3.0\n" + // TODO ps versions?
      "%%BoundingBox: 0 0 " + size.width + " " + size.height + "\n" +
      "%%EndComments\n" +
      graphInfo.toPs +
      "/Helvetica findfont\n" +
      "12 scalefont setfont\n" +
      "/top {" + size.height + "} def\n" +
      "/fontheight { % (font -- n_height) get height of font\n" +
      "  currentfont dup /FontBBox get 3 get exch /FontMatrix get 3 get mul\n" +
      "} def\n" +
      "/fontdepth { % (font -- n_depth) get depth of font\n" +
      "  currentfont dup /FontBBox get 1 get neg exch /FontMatrix get 3 get mul\n" +
      "} def\n" +
      "/showc { % (s --) show centred\n" +
      "  dup stringwidth exch 2 div neg exch pop fontdepth fontheight sub 2 div rmoveto show\n" +
      "} def\n" +
      "/showbl { % (s --) show with bottom left a current pos and some extra margin\n" +
      "  1 fontdepth 1 add rmoveto show\n" +
      "} def\n" +
      "/showbr { % (s --) show with bottom right a current pos and some extra margin\n" +
      "  dup stringwidth exch neg 1 sub exch pop fontdepth 1 add rmoveto show\n" +
      "} def\n" +
      "/showtl { % (s --) show with top left a current pos and some extra margin\n" +
      "  dup stringwidth exch pop 1 exch pop fontheight 1 add neg rmoveto show\n" +
      "} def\n" +
      "/showtr { % (s --) show with bottom right a current pos and some extra margin\n" +
      "  dup stringwidth exch neg 1 sub exch pop fontheight 1 add neg rmoveto show\n" +
      "} def\n" +
      nodes.values.map(
        x ⇒ x.toPs(this) +
          x.edgesOut.values
          .map(e ⇒ e.toPs()).mkString("\n")).mkString("\n") +
        " showpage \n"
  }
}