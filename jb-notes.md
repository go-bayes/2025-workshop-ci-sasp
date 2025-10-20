# notes
- THANKS 
- I AM
- reorder session 
- define symbols
- story 
- y(1) - y(0)






This appendix provides a glossary of common terminology in causal inference.

**Acyclic**: a causal diagram cannot contain feedback loops. More precisely, no variable can be an ancestor or descendant of itself. If variables are repeatedly measured here, it is vital to index nodes by the relative timing of the nodes.

**Adjustment set**: a collection of variables we must either condition upon or deliberately avoid conditioning upon to obtain a consistent
causal estimate for the effect of interest [@pearl2009].

**Ancestor (parent)**: a node with a direct or indirect influence on others, positioned upstream in the causal chain.

**Arrow**: denotes a causal relationship linking nodes.

**Backdoor path**: a "backdoor path" between a treatment variable, $A$, and an outcome variable, $Y$, is a sequence of links in a causal diagram that starts with an arrow into $A$ and reaches $Y$ through common causes, introducing potential confounding bias such that statistical association does not reflect causality. To estimate the causal effect of $A$ on $Y$ without bias, these paths must be blocked by adjusting for confounders. The backdoor criterion guides the selection of variables for adjustment to ensure unbiased causal inference.

**Conditioning**: explicitly accounting for a variable in our statistical analysis to address the identification problem. In causal diagrams, we usually represent conditioning by drawing a box around a node of the conditioned variable, for example, $\boxed{L_{0}}\to A_{1} \to L_{2}$. We do not box exposures and outcomes because we assume they are included in a model by default. Depending on the setting, we may condition by regression stratification, inverse probability of treatment weighting, g-methods, doubly robust
machine learning algorithms, or other methods. We do not cover such methods in this tutorial; however, see @hernan2023.

**Counterfactual**: a hypothetical outcome that would have occurred for the same individuals under a different treatment condition than the one they experienced.

**Direct effect**: the portion of the total effect of a treatment on an outcome that is not mediated by other variables within the causal pathway.

**Collider**: a variable in a causal diagram at which two incoming paths meet head-to-head. For example, if $A \rightarrowred \boxed{L} \leftarrowred Y$, then $L$ is a collider. If we do not condition on a collider (or its descendants), the path between $A$ and $Y$ remains closed. Conditioning on a collider (or its descendants) will induce an association between $A$ and $Y$.

**Confounder**: a member of an adjustment set. Notice a variable is a "confounder" in relation to a specific adjustment set. "Confounder" is a
relative concept [@lash2020].

**d-separation**: in a causal diagram, a path is "blocked" or "d-separated" if a node along it interrupts causation. Two variables are
d-separated if all paths connecting them are blocked, making them conditionally independent. Conversely, unblocked paths result in
"d-connected" variables, implying potential dependence [@pearl1995].

**Descendant (child)**: a node directly or indirectly influenced by upstream nodes (parents).

**Effect-modifier**: a variable is an effect-modifier, or "effect-measure modifie" if its presence changes the magnitude or direction of the effect of an exposure or treatment on an outcome across the levels or values of this variable. In other words, the effect of the exposure is different at different levels of the effect modifier. 

**External validity**: the extent to which causal inferences can be generalised to other populations, settings, or times, also called "Target Validity."

**Identification problem**: the challenge of estimating the causal effect of a variable by adjusting for measured variables on units
in a study. Causal diagrams were developed to address the identification problem by application of the rules of d-separation to a causal diagram.

**Indirect effect (mediated effect)**: The portion of the total effect transmitted through a mediator variable.

**Internal validity**: the degree to which the design and conduct have prevented bias, ensuring that the causal relationship observed can be confidently attributed to the treatment and not to other factors.

**Instrumental variable**: an ancestor of the exposure but not of the outcome. An instrumental variable affects the outcome only through its effect on the exposure and not otherwise. Whereas conditioning on a variable causally associated with the outcome rather than with the exposure will generally increase modelling precision, we should refrain from conditioning on instrumental variables [@cinelli2022].  Second, when an instrumental variable is the descendant of an unmeasured confounder, we should generally condition the instrumental variable to provide a partial adjustment for a confounder.

**Mediator**: a variable that transmits the effect of the treatment variable on the outcome variable, part of the causal pathway between treatment and outcome.

**Modified Disjunctive Cause Criterion**: @vanderweele2019 recommends obtaining a maximally efficient adjustment, which he calls a "confounder set." A member of this set is any set of variables that can reduce or remove structural sources of bias. The strategy is as follows:

a.  Control for any variable that causes the exposure, the outcome, or
    both.
b.  Control for any proxy for an unmeasured variable that is a shared
    cause of the exposure and outcome.
c.  Define an instrumental variable as a variable associated with the
    exposure but does not influence the outcome independently, except
    through the exposure. Exclude any instrumental variable that is not
    a proxy for an unmeasured confounder from the confounder set
    [@vanderweele2019].

Note that the concept of a "confounder set"  is broader than that of an
"adjustment set"  Every adjustment set is a member of a confounder set.
Hence, the Modified Disjunctive Cause Criterion will eliminate bias when
the data permit. However, a confounder set includes variables that 
reduce bias in cases where confounding cannot be eliminated.

**Node**: characteristic or features of units in a population (a variable) represented on a causal diagram. In a causal diagram, nodes are drawn with reference to variable distributions for the target population.

**Randomisation**: the process of randomly assigning subjects to different treatments or control groups to eliminate selection bias in experimental studies.

**Reverse causation**: $\atoyassert$, but in reality $\ytoa$

**Statistical model:** a mathematical representation of the relationships between variables in which we quantify covariances and
their corresponding uncertainties in the data. Statistical models typically correspond to multiple causal structures [@pearl2018;
@vanderweele2022b; @hernan2023]. That is, the causes of such covariances cannot be identified without assumptions.

**Structural model:** defines assumptions about causal relationships. Causal diagrams graphically encode these assumptions [@hernan2023],
leaving out the assumption about whether the exposure and outcome are causally associated. We can only
compute causal effects outside of randomised experiments with structural models. A structural model is needed to interpret the statistical findings in causal terms.
Structural assumptions should be developed in consultation with experts. The role of structural assumptions when interpreting statistical results needs to be better understood across many human sciences and forms the motivation for my work here.

**Time-varying confounding:** occurs when a confounder that changes over time acts as a mediator or collider in the causal pathway between
exposure and outcome. Controlling for such a confounder can introduce bias. Not controlling for it can retain bias.

{{< pagebreak >}}


## Appendix B: Causal Consistency in observational settings {#appendix-b}

In observational research, there are typically multiple versions of the treatment. The theory of causal inference under multiple versions of treatment proves we can consistently estimate causal effects where the different versions of treatment are conditionally independent of the outcomes [@vanderweele2009, @vanderweele2009; @vanderweele2013; @vanderweele2018] 

Let $\coprod$ denote independence.
Where there are $K$ different versions of treatment $A$ and no confounding for $K$'s effect on $Y$ given measured confounders $L$ such that

$$
Y(k) \coprod K | L
$$

Then it can be proved that causal consistency follows. According to the theory of causal inference under multiple versions of treatment, the measured variable $A$ functions as a "coarsened indicator" for estimating the causal effect of the multiple versions of treatment $K$ on $Y(k)$ [@vanderweele2009; @vanderweele2013; @vanderweele2018].  

In the context of green spaces, let $A$ represent the general action of moving closer to any green space and $K$ represent the different versions of this treatment. For instance, $K$ could denote moving closer to different green spaces such as parks, forests, community gardens, or green spaces with varying amenities and features.

Here, the conditional independence implies that, given measured confounders $L$ (e.g. socioeconomic status, age, personal values), the type of green space one moves closer to ($K$) is independent of the outcomes $Y(k)$ (e.g. mental well-being under the $K$ conditions). In other words, the version of green space one chooses to live near does not affect the $K$ potential outcomes, provided the confounders $L$ are appropriately controlled for in our statistical models.

Put simply, strategies for confounding control and consistently estimating causal effects when multiple treatment versions converge. However, the quantities we estimate under multiple treatment versions might need clearer interpretations.  For example, we cannot readily determine which of the many treatment versions is most causally efficacious and which lack any causal effect or are harmful.  

{{< pagebreak >}}

### Appendix C Simulation of Cross-Sectional Data to Compute the Average Treatment Effect When Conditioning on a Mediator {#appendix-c}

This appendix outlines a simulation designed to demonstrate the potential pitfalls of conditioning on a mediator in cross-sectional analyses. The simulation examines the scenario where the effect of access to green space ($A$) on happiness ($Y$) is fully mediated by exercise ($L$). This setup aims to illustrate how incorrect assumptions about the role of a variable (mediator vs. confounder) can lead to misleading estimates of the Average Treatment Effect (ATE).
