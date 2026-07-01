🦠 Australia's Hidden Extinction Crisis (Interactive Shiny Web App)
An end-to-end interactive data journalism and analytics application investigating the true drivers behind Australia's threatened species decline.

🔗 **Live Application:** [View the Live Dashboard ](https://divyachahal.shinyapps.io/s4119450_australia_extinction_crisis/)

## 🎯 Executive Project Summary
While habitat loss dominates mainstream headlines, expert-validated ecological data reveals a deeper crisis. This interactive web application synthesises national conservation records to demonstrate that invasive species and diseases inflict the most severe harm per species, different taxonomic groups face entirely unique ecological enemies, and the majority of threatened fauna are battling multiple simultaneous threats.

---

## 🛠️ Tech Stack & Advanced Engineering
- **Core Engine:** R & Shiny Web Framework (`ui.R` and `server.R` reactive data architectures)
- **Data Integration & Cleansing:** Blended longitudinal population datasets with national threat index metrics. Handled cross-source taxonomy mapping conventions (e.g., standardising 'Frogs' and 'Amphibians' indicators).
- **Reactive UI Design:** Implemented fully responsive HTML/Bootstrap containers via `bslib`, custom sort-toggles, and multi-tier filtering mechanics.
- **Production Deployment:** Maintained and hosted an independent cloud application container on the `` platform.

---

## 💡 Key Analytic Dimensions
- **The Collapse Matrix:** Visualises the historic drop (12% to 88%) in threatened populations since a 1985 baseline utilizing TERN data.
- **Threat-Severity Mapping:** Combines threat frequency (x-axis) against severe impact likelihood (y-axis) to isolate systemic environmental stressors.
- **Taxonomic Multi-Filters:** Allows users to filter data down to Birds, Mammals, Frogs, Reptiles, Fish, Plants, or Invertebrates—automatically re-ranking regional threats dynamically.
- **Simultaneous Threat Tracking:** Implemented statistical grouping logic to prove that single-issue conservation policies fail because the majority of endangered species face 4+ severe threat categories concurrently.

---

## 📦 Primary Data Foundations
- **Threat Profiling:** Ward, M., et al. (2021). *A national-scale dataset for threats impacting Australia's imperiled flora and fauna.* (Ecology and Evolution).
- **Population Trajectories:** TERN (2025). *Australia's Threatened Species Index (TSX)*, University of Queensland.
