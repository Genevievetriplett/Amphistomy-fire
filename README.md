These are the R scripts and data that I used to analyze and visualize my findings from "Stomatal Distribution and Post-fire Recovery: Intra- and Interspecific Variation in Plants of the Pyrogenic Florida Scrub". 
there are three experiments here:
  1. Interspecific survey of whether 116 plant species found in the Florida scrub have stomata on both leaf surfaces
     > Analyzed to see if amphistomy presence correlated with various factors
     > Subsetted a phylogenetic tree using a pre-existing one
     > Blomburg's K to test for phylogenetic signal of amphistomy
     > Did a Pagels (1994) analysis on whether it was correlated with post-fire recovery method
  2. Direct experimentation for intraspecific variation of stomatal traits
     > Sampled resprouting palmettos for stomatal ratio and density before and after either controlled burn or cutting all above-ground leaves off
     > analyzed whether there was a difference after the treatments using T-tests and linear models
  3. Indirect experiment for intraspecific variation of stomatal traits
     >  Sampled long-lived leaves from palmettos from 12 sites burned .5-50 years ago
     > constructed linear mixed-effects models using time-since-fire as a fixed effect and site as a random effect to analyze the difference in both stomatal ratio and stomatal density
     > two methods- compare within- individual response using their oldest and newest leaves, and compare across individuals, holding leaf age relatively constant

This was my first experience with both r and github, Having largely taught myself (you can probably tell). Please message me if there are any issues
