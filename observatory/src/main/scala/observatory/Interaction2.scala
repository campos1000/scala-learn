package observatory

/**
  * 6th (and last) milestone: user interface polishing
  */
object Interaction2 {

  val COLOR_SCALE = Array((7.0,	Color(0,0,0)), (4.0, Color(255, 0, 0)), (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)), (-2.0, Color(0, 255, 255)), (-7.0, Color(0, 0, 255)))

  /**
    * @return The available layers of the application
    */
  def availableLayers: Seq[Layer] = Array(new Layer(LayerName.Temperatures, COLOR_SCALE, 1990 to 2015),
    new Layer(LayerName.Deviations, COLOR_SCALE, 1990 to 2015))

  /**
    * @param selectedLayer A signal carrying the layer selected by the user
    * @return A signal containing the year bounds corresponding to the selected layer
    */
  def yearBounds(selectedLayer: Signal[Layer]): Signal[Range] = Signal(selectedLayer().bounds)

  /**
    * @param selectedLayer The selected layer
    * @param sliderValue The value of the year slider
    * @return The value of the selected year, so that it never goes out of the layer bounds.
    *         If the value of `sliderValue` is out of the `selectedLayer` bounds,
    *         this method should return the closest value that is included
    *         in the `selectedLayer` bounds.
    */
  def yearSelection(selectedLayer: Signal[Layer], sliderValue: Signal[Year]): Signal[Year] = {
    Signal(
      if (sliderValue() > selectedLayer().bounds.end) selectedLayer().bounds.end
      else if (sliderValue() < selectedLayer().bounds.start) selectedLayer().bounds.start
      else sliderValue()
    )
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The URL pattern to retrieve tiles
    */
  def layerUrlPattern(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(s"target/${selectedLayer().layerName.id}/${selectedYear()}/<zoom>/<x>-<y>.png.")
  }

  /**
    * @param selectedLayer The selected layer
    * @param selectedYear The selected year
    * @return The caption to show
    */
  def caption(selectedLayer: Signal[Layer], selectedYear: Signal[Year]): Signal[String] = {
    Signal(selectedLayer().layerName.id.capitalize + " (" + selectedYear() + ")")
  }

}

sealed abstract class LayerName(val id: String)
object LayerName {
  case object Temperatures extends LayerName("temperatures")
  case object Deviations extends LayerName("deviations")
}

/**
  * @param layerName Name of the layer
  * @param colorScale Color scale used by the layer
  * @param bounds Minimum and maximum year supported by the layer
  */
case class Layer(layerName: LayerName, colorScale: Seq[(Temperature, Color)], bounds: Range)

