# SESAM model: Redispatch with PtG 

Joulia.jl: A Large-Scale Spatial Power System Model for Julia

[Xiong, Bobby; Predel, Johannes; Crespo del Granado, Pedro; Egging-Bratseth, Ruud. 2020. "Spatial flexibility in redispatch: Supporting low carbon energy systems with Power-to-Gas". Applied Energy.](https://doi.org/10.1016/j.apenergy.2020.116201)

For the linked open access publication, we provide our model code based on JuliaLang (v1.3.1) and visualisation framework using R (v4.0.0) under the MIT licence.

# Abstract

The energy transition faces the challenge of increasing levels of decentralised renewable energy injection into an infrastructure originally laid out for centralised, dispatchable power generation. Due to limited transmissioncapacity and flexibility, large amounts of renewable electricity are curtailed. In this paper, we assess how Power-to-Gas facilities can provide spatial and temporal flexibility by shifting pressure from the electricity grid to the gas infrastructure. For this purpose, we propose a two-stage model incorporating the day-head spotmarket and subsequent redispatch. We introduce Power-to-Gas as a redispatch option and apply the model tothe German electricity system. Instead of curtailing renewable electricity, synthetic natural gas can be producedand injected into the gas grid for later usage. Results show a reduction on curtailment of renewables by 12 % through installing Power-to-Gas at a small set of nodes frequently facing curtailment. With the benefits ofdecentralised synthetic natural gas injection and usage, we exploit the advantages of coupling the two energy systems. The introduction of Power-to-Gas provides flexibility to the electricity system, while contributing toa higher effective utilisation of renewable energy sources as well as the natural gas grid

# Links

- We base our technology class definitions on [Joulia.jl](https://github.com/JuliaEnergy/Joulia.jl/) by J. Weibezahn and M. Kendziorski.
- We use the open source electricity system data set [ELMOD](https://ideas.repec.org/p/diw/diwddc/dd83.html) for Germany (2015).

