# Real-time bus model

## Contents

- Overview of bus models
- Overview of particle filtering as an estimation method than can be used in real-time
- Segmentation of routes to road segments
    - From GTFS `shapes` to `segments`
- Real-time bus model allowing for intersections and bus stop dwell times, to estimate road travel times (and therefore speed)
    - The likelihood function: vehicle positions + stop time updates
    - Challenges: getting it running in real-time on a full transit network (>1000 buses at peak times!), required
    development of a specialised C++ program that can run indefinitely (on a Virtual Machine)
- Future work: 
    - vehicle travel times vary over time, and are uncertain - need to develop a road-state model that uses travel time estimates to update state of road network
    - development of predictive framework using combination of vehicle position and road state (of upcoming segments) to generate ETAs for commuters