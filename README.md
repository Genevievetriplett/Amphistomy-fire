# Stomatal Distribution and Post-fire Recovery in Florida Scrub Plants

These are the R scripts and data I used to analyze and visualize findings from my project:

**"Stomatal Distribution and Post-fire Recovery: Intra- and Interspecific Variation in Plants of the Pyrogenic Florida Scrub."**

---

## 🔬 Overview of Experiments

This project includes three main experiments:

### 1. Interspecific Survey of Stomatal Distribution
- Surveyed 116 plant species from the Florida scrub to determine whether they have stomata on both leaf surfaces (amphistomy).
- Analyzed correlations between amphistomy and various plant traits.
- Subsetted a phylogenetic tree using a pre-existing dataset.
- Used **Blomberg’s K** to test for phylogenetic signal of amphistomy.
- Performed a **Pagel’s (1994)** analysis to test for correlation between amphistomy and post-fire recovery method.

### 2. Direct Experiment on Intraspecific Variation
- Sampled resprouting palmettos to assess changes in stomatal ratio and density before and after two treatments:
  - Controlled burn
  - Manual removal of all above-ground leaves
- Used **t-tests** and **linear models** to analyze treatment effects.

### 3. Indirect Experiment on Intraspecific Variation
- Sampled long-lived leaves from palmettos across 12 sites that burned 0.5–50 years ago.
- Built **linear mixed-effects models** with:
  - **Time-since-fire** as a fixed effect
  - **Site** as a random effect
- Two approaches:
  1. Compared oldest vs. newest leaves within individual plants.
  2. Compared across individuals, holding leaf age relatively constant.

---

## 📁 Contents

- `*.R` — All R scripts used for data cleaning, analysis, and visualization
- `data/` — Associated datasets
- `figures/` (if included) — Plots and figures generated
- `README.md` — Project summary and guidance (this file)

---

## 🚧 Notes

This was my first experience with both **R** and **GitHub**, and I taught myself as I went (as you can probably tell). If you run into any issues or have questions, feel free to reach out!

---
