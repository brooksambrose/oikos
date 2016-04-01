#Tradition and Innovation in Scientists’ Research Strategies

Jacob G.

Andrey Rzhetskyb

James A.

aUniversity of California-Los Angeles

bUniversity of Chicago

Jacob Foster, Department of Sociology, University of California-Los Angeles, 264 Haines Hall, 375 Portola Plaza, Los Angeles, CA 90095; James Evans, Department of Sociology, University of Chicago, 1126 East 59th Street, Chicago, IL 60637 E-mail: foster@soc.ucla.edu; jevans@uchicago.edu

##Abstract

What factors affect a scientist’s choice of research problem?

sociology of science tradition innovation networks generative models biomedicine citations awards field theory

#intro

Why do scientists pursue a particular research problem?

Research choice is central to many classic investigations in the sociology of science (Busch, Lacy, and Sachs 1983; Diamond 1994; Gieryn 1978; Merton 1938; Zuckerman 1978).

As the qualitative and historical literature notes, many factors influence a scientist’s choice of research problem.

These many factors can be systematically assembled using Bourdieu’s (1975, 2004) field theory of science.

Although long neglected by sociologists of science, Bourdieu’s distinctive approach has recently experienced a renaissance (Albert and Kleinman 2011; Camic 2011, 2013; Kim 2009; Panofsky 2011).

Using contemporary biomedicine as an example, we analyze the large-scale pattern of research claims and provide a strategic, dispositional account of scientific choice, drawing on the rich, published record of successful research choices and rewards.

These conflicting demands create a tension between two broad strategies: productive tradition and risky innovation (Kuhn [1959] 1977).3 When following a conservative strategy and adhering to a research tradition in their domain, scientists achieve publication with high probability: they remain visibly productive, but forgo opportunities for originality.

In this article, we explore the essential tension at scale, studying 6.5 million abstracts in biomedicine.

Our empirical analysis follows six steps: (1) we show how networks can be used to map the evolving landscape of chemical knowledge in biomedicine; (2) we identify clusters of knowledge within that map and demonstrate their stability; (3) we define a simple structural typology of research strategies corresponding to tradition and innovation, and we show that the distribution of published strategies is remarkably stable; (4) we use a simple probabilistic model to measure the broad habits of perception, attention, and choice—the habitus—that constrain research activity and support stability; (5) we quantify the relationship between strategy, risks, and rewards (citations) using several regression models; and (6) we explore citation accumulation and awards as incentives that discipline scientists’ choices and help structure the field.

This article makes distinct contributions to the sociology of science, the study of networks, and quantitative methodology.

#The Essential Tension and Scientific Habitus

Kuhn ([1959] 1977) introduced the notion of the essential tension at a conference motivated by Cold War concerns about declining originality, innovation, and scientific competitiveness in the United States (a persistent concern; see Cowen 2011).

Kuhn challenged the privileged role of divergent thinking in his remarks.

For Kuhn ([1959] 1977:229), the primary mechanism that maintains tradition is education: the solution of textbook problems “that the profession has come to accept as paradigms.” Although Kuhn ([1959] 1977:227) acknowledged that tradition is “reinforced by subsequent life in the profession,” Kuhn’s scientists are above all trained to work within a tradition—to ignore most of the anomalies churned up by daily work.

A more convincing version of the essential tension is presented in Bourdieu’s early writing on the sociology of science.

Although the use of tradition or innovation strategies in pursuit of recognition should reflect the specificity of habitus, position, capital, opportunity, and risk, Bourdieu often suggests a “direct correspondence between an agent’s field position—dominant vs.

Here we propose three ways in which strategic dispositions of the scientific habitus are formed and adapted to the accumulation of scientific capital; we will analyze all three quantitatively.

Stepping back from this theoretical account, let us paint a practical picture linking individual action to field structure: A scientist must decide what to work on next.

Research on the sociology, economics, and management of innovation independently grounds several aspects of this story.

Management scholars Bateman and Hess (2015: Supporting Information p.

Finally, research has elucidated the role of awards in stimulating scientists to take risks (Wright 1983).

The evidence and theory outlined here suggest that most published findings in well-developed fields should be expected and unsurprising.11 Such findings fit with tradition: scientists with the appropriate habitus are disposed to generate and acknowledge them as valid science.

In this article, we examine these claims quantitatively, in the context of biomedicine.

##Knowledge Networks and Network Strategies

The document is the fundamental unit of analysis in most large-scale quantitative studies of scientific behavior (Cronin and Atkins 2000; Menczer 2004).

We propose the following coarse taxonomy of research strategies, corresponding to structurally distinct contributions to the network of scientific knowledge (Newman 2003).

- Table 1.
- Largest Chemical Clusters Induced by Map Equation from the MEDLINE Chemical Term Network

- Figure 1.
- Scientific Strategies in a Network
- Note: Nodes represent chemicals and links represent chemical relationships.

#Data, Methods, and Results

Our analysis follows six distinct steps and connects to the vignette of our practicing scientist, described earlier.

Note that our data reveal only the outcome of successful choices, that is, published papers.

##Step 1: Chemistry Data and Network Construction

Our network is built from 6,455,756 abstracts in the NIH National Library of Medicine’s (NLM) MEDLINE collection, published from 1934 to 2008 and annotated by NLM with two or more chemical entities (see Part A of the online supplement [http://asr.sagepub.com/supplemental]).

Our procedure maps the rapidly growing network of chemical knowledge—published tradition—atop the space of possible chemical relationships.

This first step provides us with a detailed map of tradition for any given year in contemporary biomedical chemistry (1983 to 2008).

##Step 2: Community Detection and Knowledge Clusters

The social organization of scientific attention means that published chemical relationships bunch into clusters of chemicals that are frequently investigated together.

###Community detection algorithm

Community detection algorithms reveal intermediate structures in large, complex networks (Bruggeman, Traag, and Uitermark 2012; Shwed and Bearman 2010).

The map equation provides a community detection scheme that precisely mirrors this picture (Rosvall et al.

###Knowledge clusters

To extract the knowledge clusters used to categorize links added at time t, we consider the entire network uncovered in all years prior to t.

Our community detection procedure discovers scientifically plausible knowledge clusters in this network.

- Figure 2.
- Largest MEDLINE Knowledge Clusters, 1983 to 2003
- Source: Using the map equation algorithm and associated alluvial and network diagram software (Rosvall and Bergstrom 2008, 2010).
- Note: Here we show an alluvial diagram for the chemical network at four time points: 1983, 1990, 1996, and 2003.

On the whole, knowledge clusters are quite stable.

Knowledge clusters change over time as shifts in scientific attention and publication add new chemicals, move chemicals to another cluster, merge clusters together, or split them apart.

Table 1 lists the top 10 chemicals (by PageRank) for each knowledge cluster in the 2003 network (shown in Figure 2).

Now that we have extracted the knowledge clusters for each year, we can fully define the space of possible research questions and sort them into our taxonomy of research strategies.

##Step 3: Prevalence and Stability of Strategies

###Strategy definitions

Our taxonomy of strategies, defined briefly earlier, is specified as follows:22 jumps involve at least one chemical that joined the network in the current year t; new consolidations connect known chemicals from the same knowledge cluster with no link between them in year t – 1; new bridges connect known chemicals from different knowledge clusters with no link between them in year t – 1; repeat consolidations reconnect chemicals from the same cluster that already have a link between them in year t – 1; and repeat bridges reconnect chemicals from different clusters that already have a link between them in year t – 1.

###Strategy frequency

Pooling counts across all years, we find that the frequency of each strategy in the published literature from 1983 to 2008 is inversely related to its plausible risk of failure, as expected for a mature science.

###Yearly strategy distributions

We refer to the relative frequency of strategies within a year as the strategy distribution (see Figure 3A, solid lines).

- Figure 3.
- Stable Strategies and Dynamical Attention
- Note: (A) The empirical frequency of each strategy (solid line) with 95 percent confidence intervals smaller than the solid lines.

The observed prevalence of each research strategy reveals aspects distinctive to the development of biomedical chemistry.

Linear regressions of frequency on year show no significant temporal trend for new bridges, and significant but slight trends for jumps and new consolidations; their shifts each alter the mix of strategies less than 1 percent over the study period (see Part C and Table S2 in the online supplement).

This stability in the strategy distribution is surprising given the enormous changes in biomedical chemistry over the past few decades.

To explore this further, in the next section we develop a simple generative behavioral model that relates the opportunities to pursue each strategy to the observed distribution of strategies.

##Step 4: Connecting Stability to Attention; A Generative Model of the Biomedical Habitus

The stability of the strategy distribution, despite dramatic expansions in research opportunity, implies that scientific attention is narrowing in focus.

We assume that scientists choose probabilistically from the five possible strategies: jump, new consolidation, new bridge, repeat consolidation, and repeat bridge.

as the five strategies partition the sample space.

Once a scientist chooses new or repeat, she must either consolidate or bridge conditional on that choice:

Substituting this into the previous equation, we have the following:

In other words, extreme innovation, innovation, and tradition provide a coarse partition of the sample space.

Researchers, here modeled by a representative agent,25 independently and randomly choose a strategy at time t based on the number of possible links in the network corresponding to each strategy at that time.

where Formula is the bias for new relationships, Formula the bias for repeats, Formula for new consolidation, and Formula for repeat consolidation.

Conceptually, we imagine that the agent repeats this procedure until it has made the same number of choices as we observe in a given year.

The likelihood function depends on the bias parameters through the strategy probabilities (see previous equation).

Figure 3A (dashed line) shows that this behavioral model predicts observed behavior reasonably well, with a high correlation between known and predicted values (Pearson’s R = .983).

In our Bourdieusian account of scientific choice, the relevant dispositions must be shaped by more than training or observed strategy prevalence.

##Step 5: Measuring the Relationship between Strategy, Risk, and Reward

We now connect the disposition to pursue tradition (repeat links) over innovation (new and jump links) to the reward mechanisms that drive scientists in their pursuit of recognition.

###Publication-level strategies

Careful analysis of the relationship between strategy, risk, and reward demands a taxonomy that classifies each publication uniquely.

###Surprisal

In information theory, self-information or surprisal measures the information associated with observing outcome i of a discrete random variable (Cover and Thomas 1991).

###Citation counts

Citations are assigned to abstracts by linking MEDLINE abstracts to the ThomsonReuters Web of Science citation database.

###Aggregate analysis: strategy-year models

In these models, we pool all abstracts corresponding to each publication-level strategy in a given year and compute the mean citations and the standard deviation in citations for that pool.

###Aggregate results

Results from these models confirm our conjecture that strategy surprisal positively varies with mean citations, conditional on that strategy being published.

We also confirm our conjecture that surprisal is positively correlated with the uncertainty of reward, which we operationalized as the standard deviation in citations.

- Table 2.
- Models Regressing Citations on Strategy Surprisal or Type

- Figure 4.
- Strategies and Citation Impact
- Note: (A) Rare innovation strategies are correlated with higher mean citations (49 percent of variation explained).

###Article-level analysis

Because each article can be associated with its distinguishing strategy, we can also test the association between strategies and citations on disaggregated article-level data, accounting directly for overdispersion in citations (article level of analysis in Table 2).

To test the effect on citations of strategy type, we assume the following:

where Formula is the number of citations received by article a in the year a was published and the subsequent three years.

where strategy surprisal is assigned as above.

where Formula , Formula , and such are indicator variables; Formula is a continuous predictor; and Formula , the error, is assumed to be a gamma distributed random variable.

where Formula and Formula are indicator variables and Formula and Formula are treated as before.

###Article-level results

Our results for this analysis are summarized in Table 2.

###Optimal strategy analysis

We also use negative binomial regressions to test a related hypothesis, connecting expected citations to the fraction of links in a given article that correspond to a particular link-level strategy.

We use negative binomial regressions to test the connection between expected citations and the fraction of links in a given paper that correspond to a particular link-level strategy.

where Formula refers to the proportion of chemical relationships in article a that represent jumps; Formula is the same quantity squared; and Formula , the error, is again assumed to be a gamma distributed random variable.

###Optimal strategy results

Negative binomial regressions of expected article citations on strategy fraction and fraction-squared bear out our intuition that there should be a robust unimodular relationship between strategy proportion and expected citations (see Part F and Table S4 in the online supplement); Figure 4B depicts that relationship.

At first blush, these proportions appear to disagree with Uzzi and colleagues’ (2013) recent finding that peak impact is associated with articles in the 85th to 95th percentile of median conventionality in their references, implying an overwhelming number of repeat reference combinations.

We have demonstrated that innovation is more richly rewarded with citations than is tradition.

##Step 6: Analyzing Possible Motivations

###Maximizing citations

We verified our conjecture that innovation strategies should be more highly rewarded, on average, than tradition strategies.

This analysis requires that we estimate the chance of utter failure, in which nothing is found or the resulting paper fails to pass peer review.

To analyze whether citation maximization is a credible motivation for the observed distribution of scientists’ research choices, we again study paper-level strategies, now taking into account the a priori probability that a particular strategy yields a publication.

For a risk-neutral, citation-maximizing scientist to be indifferent between strategies—for the choice of new or jump instead of repeat to be rational—then E(citations | repeat) = E(citations | new) = E(citations | jump).

This result suggests that the probability of reaching publication for innovation strategies is sufficiently small that repeat is likely to be the dominant strategy for a scientist seeking to maximize expected citations on each project.

###Exceptional achievement and prizewinners

Beyond job security, scientists are also motivated by the desire for significant impact and recognition.

To analyze the distribution of strategies among award-winning scientists, we compiled a list of 137 prizes and awards in biomedicine and chemistry, drawing on the category pages for biology awards, medicine awards, and chemistry awards on Wikipedia.43 We confirmed the resulting list with several biology, medical, and chemistry researchers.

All prestigious and many less prestigious prizes are listed, providing a broad sample of achievement—from field-specific (e.g., the Anselme Payen award, related to cellulose chemistry) to world-recognized (e.g., the Nobel Prize in Physiology or Medicine).

To examine truly outstanding achievement, we compiled a list of “elite, general” prizes (see Figure 5B).

- Figure 5.
- Strategies Shift for High Impact Scientists
- Note: (A) Pooling all papers published from 1983 to 2002, we find a distribution of strategies similar to that observed in any year (Figure 3A).

Figure 5 verifies with an aggregate analysis that top-cited articles deploy significantly more innovation strategies than all articles as a fraction of links contributed, with the strongest enrichment in jumping and new consolidation.

Articles written by authors who won one of 137 different prizes in biomedicine and chemistry show a similar pattern of enrichment (see Figure 5B).

These results provide a plausible motivational mechanism for the other half of the essential tension, linking risky innovation to extraordinary scientific achievement.

#Discussion

##Strategic Origins of the Essential Tension

Taken together, our results provide strong quantitative evidence that scientists’ choice of research problem is indeed shaped by their strategic negotiation of an essential tension between productive tradition and risky innovation (Kuhn [1959] 1977).

###Limitations of the Current Study and Robustness of Results to Selection

####Broad limitations

Our analysis builds on much recent research in the social science of innovation but draws together many effects in a single case; it thus serves as the first large-scale confirmation of a key concept in the qualitative study of science and innovation.

####Selection effect

Strategies that failed to yield publishable findings are excluded from our analysis because we focus on publications.

In fact, our results are largely unaffected by this filtering process.

The only claim directly affected by the selection effect is the relative rank of strategy prevalence in science.

#Conclusions

Understanding the research process remains a central challenge for science studies, and improved understanding will be key to improved science policy (Evans and Foster 2011).

To be sure, not all scientists should pursue risky strategies.

Our paper also has implications for the sociology of science and for sociological methodology more broadly.

As we noted earlier, the essential tension has been studied many times under many names.

Methodologically, our analysis makes three contributions.

#Acknowledgments

We are grateful for helpful comments from Carl Bergstrom, David Blei, Charles Camic, Erica Cartmill, and Steve Epstein.

  