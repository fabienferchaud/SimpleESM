# SimpleESM
A simple R script to calculate soil organic carbon and nitrogen stocks at Equivalent Soil Mass (ESM)

Contact email: fabien.ferchaud@inrae.fr

To calculate the stock of a given element in the soil, it is necessary to know the element concentration and the soil mass per unit area. When stocks are calculated at fixed depth (FD), a change in bulk density leads to a change in the calculated stock, even without any change in the concentrations. This can lead to significant misinterpretation of data, especially when comparing agricultural practices resulting in changes in bulk density in (part of the) the soil profile (e.g. conventional tillage versus no-till). To avoid such misinterpretation, stocks can be calculated at equivalent soil mass (ESM).

The "SimpleESM" R script aims to facilitate calculation of soil organic carbon and nitrogen stocks at ESM using concentration and bulk density data in multiple soil layers.
