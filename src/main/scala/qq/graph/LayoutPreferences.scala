package qq.graph

import qq.util.binding.PropertyOwner
import qq.util.binding.Property
import qq.util.binding.Restriction
import java.util.prefs.Preferences

/** Preferences that control the layout algorithm */
class LayoutPreferences
extends PropertyOwner {
  
  private val prefs = Preferences.userRoot().node(s"/qq/skilledit/layout");  
  
  val c1: Property[Float] = new Property(this,
      "Edge force scale factor (c1)", prefs.getFloat("c1", 2.0f))
  c1.onChange.strong += (prefs.putFloat("c1", _))
  val c3: Property[Float] = new Property(this,
      "Node-node repulsion force scale factor (c3)", prefs.getFloat("c3",8000.0f))
  c3.onChange.strong += (prefs.putFloat("c3", _))
  val c2: Property[Float] = new Property(this,
      "Desired edge length in pixels (c2)", prefs.getFloat("c2",100.0f))
  c2.onChange.strong += (prefs.putFloat("c2", _))
  val c4: Property[Float] = new Property(this,
      "Edge direction force scale factor (c4)", prefs.getFloat("c4", 2.0f))
  c4.onChange.strong += (prefs.putFloat("c4", _))
  val c5: Property[Float] = new Property(this,
      "Smoothing factor for learning edge directions (c5)", prefs.getFloat("c5",0.9f)) {
    restrictions += Restriction.min(0.0f)
    restrictions += Restriction.max(1.0f)
  }
  c5.onChange.strong += (prefs.putFloat("c5", _))

  val scaleDirectionWhenConflict: Property[Boolean] = new Property(this,
      "scale down edge direction force when same field edges meet", prefs.getBoolean("scaleDirectionWhenConflict",true)) 
  scaleDirectionWhenConflict.onChange.strong += (prefs.putBoolean("scaleDirectionWhenConflict", _))
  val ε: Property[Float] =  new Property(this,
      "Minimum edge length in pixels used for calculation when nodes overlap (ε)", prefs.getFloat("epsilon", 5f))
  ε.onChange.strong += (prefs.putFloat("epsilon", _))
  val margin: Property[Float] = new Property(this,
      "Margin of the frame", prefs.getFloat("margin", -30f))
  margin.onChange.strong += (prefs.putFloat("margin", _))
  val cluttered: Property[Float] = new Property(this,
      "`Energy' threshold per node for cluttered layouts", prefs.getFloat("cluttered", 5.0f))
  cluttered.onChange.strong += (prefs.putFloat("cluttered", _))
  val rootAtCentre: Property[Boolean] = new Property(this,
      "Fix position of root node to centre", prefs.getBoolean("rootAtCentre",true)) 
  rootAtCentre.onChange.strong += (prefs.putBoolean("rootAtCentre", _))
  val rootBold: Property[Boolean] = new Property(this,
      "Use bold border for root", prefs.getBoolean("rootBold",true)) 
  rootBold.onChange.strong += (prefs.putBoolean("rootBold", _))
  val initialIterations: Property[Int] = new Property(this,
      "Iterations without overlap avoidance", prefs.getInt("initialIterations", 50))
  initialIterations.onChange.strong += (prefs.putInt("initialIterations", _))
  val phaseInIterations: Property[Int] = new Property(this,
      "Iterations during which overlap avoidance is phased in", prefs.getInt("phaseInIterations", 50))
  phaseInIterations.onChange.strong += (prefs.putInt("phaseInIterations", _))
  val finalIterations: Property[Int] = new Property(this,
      "Iterations with overlap avoidance fully active", prefs.getInt("finalIterations", 50))
  finalIterations.onChange.strong += (prefs.putInt("finalIterations", _))
  def iterations = initialIterations() + phaseInIterations() + finalIterations()
}