#Draw features on a blank slate

library(leaflet.extras)

leaflet() %>%
  setView(0,0,2) %>%
  addTiles() %>%
  addDrawToolbar(
    targetGroup='draw',
    editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
  addLayersControl(overlayGroups = c('draw'), options =
                     layersControlOptions(collapsed=FALSE)) %>%
  addStyleEditor()

