Given a list of place files, ``--average`` normalizes the mass in each, then
takes the average of those masses to make an appropriately averaged fat tree
for those place files.

Expected Distance between Placement Locations (EDPL) values can be visualized
on the tree by specifying a maximum value for the scale via the ``--edpl``
flag. The scale is represented by 0 EDPL being white, going to red for the
maximum value. When EDPL is above the specified maximum, the branch is colored
yellow. When there are multiple placements on a given edge, their EDPL values
are combined using a weighted average, where the weights are the values of the
ML weight ratio or posterior probability.
