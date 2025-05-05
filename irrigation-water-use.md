# Irrigation Water Use

2025-03-31

# Introduction

**Irrigation Water Use** ($IWU$) is an efficiency metric, representing
the amount of water applied (unit = acre-inch) for each incremental
increase in crop yield attributed to irrigation. Lower $IWU$ values
result from either using less irrigation water or from greater yield
returns due to irrigation.

This metric is calculated only for users who irrigate. No water
efficiency metric applies to rainfed crop production.

# Methods

The water applied is determined from the total amount from all sources.
Estimates for non-irrigated yield can be as low as zero, or a total
failure of the crop. For example with rice, users may enter water
applied from more than one irrigation source. If so, sum the total water
applied from all sources to get $W$. For rice, non-irrigated yield is
assumed to be zero and does not need to be entered by the user.

While still a work in progress, Field to Market wants to support users
by developing better methods to estimate non-irrigated yields.

## Inputs

| Input | Value | Units | Symbol |
|----|----|----|----|
| Irrigated yield[^1] | User entry, converted to standard moisture |  | $Y_i$ |
| Non-irrigated yield[^2] | User estimate of yield had there been no irrigation; converted to standard moisture |  | $Y_0$ |
| Total irrigation amount | Annual irrigation water applied from all sources | acre-inch | $W$ |
| Conversion factors for acre-inch to cubic feet | 3630 | acre-inch/ft<sup>3</sup> |  |
| Conversion factors for cubic feet to cubic meters | 0.0283168 | ft<sup>3</sup>/m<sup>3</sup> |  |

## Formula

$$IWU = \frac{W}{Y_i - Y_0}$$

### **Supporting R function**

``` r
# function for demonstrating Irrigation Water Use calculation
# w  = total water applied from all sources,
# yi = yield with irrigation
# y0 = yield without irrigation (default = 0)

water_use <- function(w, yi, y0 = 0) {
  # Calculate irrigation water use (iwu)
  iwu = w / (yi - y0)
  
  return(iwu)
}
```

# Example: Soybean🌱

An irrigated soybean field with an irrigated yield of 60 bu/acre and an
estimate of non-irrigated yield of 45 bu/acre and 12 acre-inches of
water applied.

``` r
# Inputs
yield_adjusted_uscs_irrigated <- 60  # standardized yield with irrigation
yield_adjusted_uscs_not_irrigated <- 45  # standardized yield without irrigation

gross_water_pumped <- 12    # water use (acre-inch)

# Calculation
iwu = water_use(w = gross_water_pumped,
                yi = yield_adjusted_uscs_irrigated,
                y0 = yield_adjusted_uscs_not_irrigated)

iwu_metric <- water_use(w = gross_water_pumped * acreinch_m3,
                        yi = yield_adjusted_uscs_irrigated * 60 * lb_kg / ac_ha,
                        y0 = yield_adjusted_uscs_not_irrigated * 60 * lb_kg / ac_ha)
```

    [1] "0.8 acre-inch for each additional bushel/acre"

    [1] "1.22 m^3 for each additional kg/ha"

    [1] "1,223 m^3 for each additional Mg/ha"

# Example: Rice🌾

A rice field with a standardized irrigated yield of 70 cwt from two
sources of water: 18 acre-inch were sourced from surface water 6
acre-inch were sourced from groundwater.

``` r
# Calculation
iwu <- water_use(w = 18 + 6,
                 yi = 70 * 100) # convert to lbs

iwu_metric <- water_use(w = (18 + 6) * acreinch_m3,
                        yi = 70 * 100 * lb_kg / ac_ha)
```

    [1] "0.0034 acre-inch for each additional lb/acre"

    [1] "0.314 m^3 for each additional kg/ha"

# Change Log

- Examples now include metric output.

# Internal Use Only

> *The section is for Field to Market staff only and is hidden in the
> public documentation. Public-facing doc begins with
> [Introduction](#introduction).*

<table style="width:99%;">
<caption>Release history</caption>
<colgroup>
<col style="width: 8%" />
<col style="width: 6%" />
<col style="width: 47%" />
<col style="width: 36%" />
</colgroup>
<thead>
<tr>
<th>Platform Version</th>
<th>Release Date</th>
<th>Log</th>
<th>References</th>
</tr>
</thead>
<tbody>
<tr>
<td></td>
<td>2017-10-06</td>
<td><p>Appears to have been reviewed in 2016-2017. Documentation last
prepared by Allison.</p>
<p>Many notes<a href="#fn1" class="footnote-ref" id="fnref1"
role="doc-noteref"><sup>1</sup></a> are available in a Irrigation Water
Use folder within the Metrics Committee Microsoft Group.</p></td>
<td><a
href=".\archive/irrigation-water-use/irrigation-20171006-technical.pdf">2017-10-06
PDF📄</a></td>
</tr>
</tbody>
</table>
<section id="footnotes" class="footnotes footnotes-end-of-document"
role="doc-endnotes">
<hr />
<ol>
<li id="fn1"><p>Any adjustments made to crop yield calculations to
reconcile moisture content (rice, alfalfa) or sugar content (sugar
beets) should also be made to the “non-irrigated yield”. Yields must be
in the same units prior to calculation of the Irrigation Water Use
metric. See Step 1 of Land Use metric documentation.<a href="#fnref1"
class="footnote-back" role="doc-backlink">↩︎</a></p></li>
</ol>
</section>

## Technical documentation

A flowchart diagram can be [found
here](https://github.com/Field-to-Market/fpp.calculator/blob/main/docs/metrics/WaterUse/irrigation-water-use.md#flowchart).

## Potential Issues

- USDA stopped asking for non-irrigated yield in the 2018 census of ag
  and is a terminal decision. That non-irrigated number was the heart of
  the current Irrigation water use metric, and without it, the metric’s
  future is over.

- The irrigation water use metric is not about total water use, rather
  it is about marginal cost of production. As such the interpretation of
  the output (amount of water per additional yield unit) can mislead in
  sustainability reporting concerning water use.

  Some issues:

  - **Can be unclear why a value is relatively low or high**

    - Is it because non-irrigated yields were so low? It rained more
      than usual? Recently adopted variable rate tech?

    - Can get lower values in water scarce areas and higher values in
      higher rainfall areas.

      - Values in semi-arid regions look favorable because yield is so
        low without irrigation. Irrigating means big yield differences,
        and the low IWU value would imply “Irrigation is worth it” (a
        little water for a big return) even if the region doesn’t have a
        sustainable supply of water

  - **Lacks decision support**

    - Cannot really say “ if IWU goes above value X, then stop
      irrigating”

    - Cannot say “ you should not be irrigating”

  - **Data quality of non-irrigated yield**

    - IWU depends on quality data from non-irrigated field corners or
      adjacent fields or NASS-type irrigation surveys

  - Marginal cost is a useful metric for decision making, but in the
    context of water use sustainability, **IWU may be distracting from
    total water use**

    - IWU does not account for water scarcity

      - e.g., irrigating in a region with depleted aquifers or from
        rivers that run dry from maximized exercising of water rights

      - Pre- or post-harvest water may or may not be accounted for

    - IWU does not consider landscape level effects

      - Broader picture of water flowing through the landscape and how
        the quantity and quality is affected by local irrigation

    - IWU does not consider water source

      - Pulling from surface waters vs groundwaters

[^1]: Any adjustments made to crop yield calculations to reconcile
    moisture content (rice, alfalfa) or sugar content (sugar beets)
    should also be made to the “non-irrigated yield”. Yields must be in
    the same units prior to calculation of the Irrigation Water Use
    metric.

[^2]: Can be zero.
