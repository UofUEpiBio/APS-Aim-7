APS Aim 7 Derived Variables Summary
================

**NOTE:** Data is filtered to include only rows where `event_label` is
“Daily In-Hospital Forms”. This is because the variables used to compute
the following derived variables are only collected in this event.

## Neuromuscular Blockade

- 0 = No neuromuscular blockade on day 0 or missing (NA)
- 1 = Neuromuscular blockade given on day 0

| Value | Counts | %      |
|:------|-------:|:-------|
| 0     |    482 | 96.59% |
| 1     |     17 | 3.41%  |

## Inflammatory Profile (Streamlined)

- 0 = Day 0 CRP not checked or \< 15 or missing (NA)
- 1 = Day 0 CRP checked and ≥ 15

| Value | Counts | %      |
|:------|-------:|:-------|
| 0     |    470 | 94.19% |
| 1     |     29 | 5.81%  |

## Respiratory Failure Severity (Systematic)

### Variable 1

- 0 = No oxygen or missing (NA)
- 1 = Standard flow
- 2 = HFNC
- 3 = NIV
- 4 = IMV and PEEP \< 12
- 5 = IMV and PEEP ≥ 12

| Value | Counts | %      |
|:------|-------:|:-------|
| 0     |     69 | 13.83% |
| 1     |     98 | 19.64% |
| 2     |    103 | 20.64% |
| 3     |     30 | 6.01%  |
| 4     |    179 | 35.87% |
| 5     |     19 | 3.81%  |
| NA’s  |      1 | 0.2%   |

### Variable 2

- If Variable 1 is 0 = daily_spo2_8a_0 / 0.21
- If Variable 1 is 1 = daily_spo2_8a_0 / 0.21 + (0.03 \*
  daily_standard_flow_8a_0)
- If Variable 1 is 2, 3, 4, or 5 = daily_spo2_8a_0 / daily_fio2_lowest_0

![](aim7-summary_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    #>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    #>   33.33  182.18  247.50  267.35  351.85  476.19      60

**NOTE:** NA values indicate missing data for the ratio calculation.

## Hypotension Severity

- 0 = No pressors or inotrope, MAP \>= 70
- 1 = No pressors or inotrope, MAP \< 70
- 2 = (dobutamine OR milrinone) and (no dopamine or dopamine \<= 5) and
  (no other vasopressors)
- 3 = On vasopressors with NEE \< 0.1
- 4 = NEE \> 0.1 and NEE \< 0.25
- 5 = NEE \>= .25

| Value | Counts | %      |
|:------|-------:|:-------|
| 0     |    177 | 35.47% |
| 1     |     23 | 4.61%  |
| 3     |    227 | 45.49% |
| 4     |     47 | 9.42%  |
| 5     |     25 | 5.01%  |
