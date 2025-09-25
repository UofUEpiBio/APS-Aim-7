APS Aim 7 Derived Variables Summary
================

## Streamlined DAG: Neuromuscular Blockade

- 0 = No neuromuscular blockade on Day 0 or missing
- 1 = Neuromuscular blockade given on Day 0

### Code to Generate Variable

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate(str_nmblockade_0 = calc_str_nmblockade_0(
    daily_paralysis_0 = daily_paralysis_0,
    trx_0 = trx_0
  )) |>
  select(str_nmblockade_0)
#> # A tibble: 499 × 1
#>    str_nmblockade_0
#>               <dbl>
#>  1                0
#>  2                0
#>  3                0
#>  4                0
#>  5                0
#>  6                0
#>  7                0
#>  8                0
#>  9                0
#> 10                0
#> # ℹ 489 more rows
```

Note, filtering by ‘Daily In-Hospital Forms’ returns only 499 entries,
not 500?

### Variable Summary

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate(str_nmblockade_0 = calc_str_nmblockade_0(
    daily_paralysis_0 = daily_paralysis_0,
    trx_0 = trx_0
  )) |>
  count(str_nmblockade_0) |>
  gt()
```

<div id="pmkjullcao" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pmkjullcao table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pmkjullcao thead, #pmkjullcao tbody, #pmkjullcao tfoot, #pmkjullcao tr, #pmkjullcao td, #pmkjullcao th {
  border-style: none;
}
&#10;#pmkjullcao p {
  margin: 0;
  padding: 0;
}
&#10;#pmkjullcao .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pmkjullcao .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#pmkjullcao .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#pmkjullcao .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#pmkjullcao .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#pmkjullcao .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pmkjullcao .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pmkjullcao .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#pmkjullcao .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pmkjullcao .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#pmkjullcao .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#pmkjullcao .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pmkjullcao .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pmkjullcao .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#pmkjullcao .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pmkjullcao .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#pmkjullcao .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pmkjullcao .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pmkjullcao .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pmkjullcao .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pmkjullcao .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pmkjullcao .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pmkjullcao .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pmkjullcao .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#pmkjullcao .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pmkjullcao .gt_left {
  text-align: left;
}
&#10;#pmkjullcao .gt_center {
  text-align: center;
}
&#10;#pmkjullcao .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pmkjullcao .gt_font_normal {
  font-weight: normal;
}
&#10;#pmkjullcao .gt_font_bold {
  font-weight: bold;
}
&#10;#pmkjullcao .gt_font_italic {
  font-style: italic;
}
&#10;#pmkjullcao .gt_super {
  font-size: 65%;
}
&#10;#pmkjullcao .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pmkjullcao .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pmkjullcao .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pmkjullcao .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pmkjullcao .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pmkjullcao .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pmkjullcao .gt_indent_5 {
  text-indent: 25px;
}
&#10;#pmkjullcao .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#pmkjullcao div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="str_nmblockade_0">str_nmblockade_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="str_nmblockade_0" class="gt_row gt_right">0</td>
<td headers="n" class="gt_row gt_right">480</td></tr>
    <tr><td headers="str_nmblockade_0" class="gt_row gt_right">1</td>
<td headers="n" class="gt_row gt_right">17</td></tr>
    <tr><td headers="str_nmblockade_0" class="gt_row gt_right">NA</td>
<td headers="n" class="gt_row gt_right">2</td></tr>
  </tbody>
  &#10;
</table>
</div>

### Missingness Report for `daily_paralysis_0`

#### Valid NA Values

These records were missing `daily_paralysis_0`, but this was *expected*
by the branching logic of `trx_0 == 'Not Available'`. These values were
set to `neuromuscular_blockade_0 = 0` according to the variable
definition.

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_paralysis_0) & (trx_0 == 'Not Available')) |>
  select(record_id, daily_paralysis_0, trx_0) |>
  gt()
```

<div id="edqhuzxqjz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#edqhuzxqjz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#edqhuzxqjz thead, #edqhuzxqjz tbody, #edqhuzxqjz tfoot, #edqhuzxqjz tr, #edqhuzxqjz td, #edqhuzxqjz th {
  border-style: none;
}
&#10;#edqhuzxqjz p {
  margin: 0;
  padding: 0;
}
&#10;#edqhuzxqjz .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#edqhuzxqjz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#edqhuzxqjz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#edqhuzxqjz .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#edqhuzxqjz .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#edqhuzxqjz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#edqhuzxqjz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#edqhuzxqjz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#edqhuzxqjz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#edqhuzxqjz .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#edqhuzxqjz .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#edqhuzxqjz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#edqhuzxqjz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#edqhuzxqjz .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#edqhuzxqjz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#edqhuzxqjz .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#edqhuzxqjz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#edqhuzxqjz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#edqhuzxqjz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#edqhuzxqjz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#edqhuzxqjz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#edqhuzxqjz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#edqhuzxqjz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#edqhuzxqjz .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#edqhuzxqjz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#edqhuzxqjz .gt_left {
  text-align: left;
}
&#10;#edqhuzxqjz .gt_center {
  text-align: center;
}
&#10;#edqhuzxqjz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#edqhuzxqjz .gt_font_normal {
  font-weight: normal;
}
&#10;#edqhuzxqjz .gt_font_bold {
  font-weight: bold;
}
&#10;#edqhuzxqjz .gt_font_italic {
  font-style: italic;
}
&#10;#edqhuzxqjz .gt_super {
  font-size: 65%;
}
&#10;#edqhuzxqjz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#edqhuzxqjz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#edqhuzxqjz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#edqhuzxqjz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#edqhuzxqjz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#edqhuzxqjz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#edqhuzxqjz .gt_indent_5 {
  text-indent: 25px;
}
&#10;#edqhuzxqjz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#edqhuzxqjz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_paralysis_0">daily_paralysis_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="trx_0">trx_0</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">33-0007</td>
<td headers="daily_paralysis_0" class="gt_row gt_center">NA</td>
<td headers="trx_0" class="gt_row gt_center">Not Available</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0012</td>
<td headers="daily_paralysis_0" class="gt_row gt_center">NA</td>
<td headers="trx_0" class="gt_row gt_center">Not Available</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td>
<td headers="daily_paralysis_0" class="gt_row gt_center">NA</td>
<td headers="trx_0" class="gt_row gt_center">Not Available</td></tr>
  </tbody>
  &#10;
</table>
</div>

#### Invalid NA Values

These records were missing `daily_paralysis_0`, but this was
*unexpected* by the branching logic.

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_paralysis_0) & (trx_0 %in% c('Available', NA))) |>
  mutate(missingness = case_when(
    trx_0 == 'Available' ~ 'By branching logic, "daily_paralysis_0" should have a value',
    is.na(trx_0) ~ '"trx_0" is also missing'
  )) |>
  select(record_id, daily_paralysis_0, trx_0, missingness) |>
  gt()
```

<div id="bzjkkppqtv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bzjkkppqtv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#bzjkkppqtv thead, #bzjkkppqtv tbody, #bzjkkppqtv tfoot, #bzjkkppqtv tr, #bzjkkppqtv td, #bzjkkppqtv th {
  border-style: none;
}
&#10;#bzjkkppqtv p {
  margin: 0;
  padding: 0;
}
&#10;#bzjkkppqtv .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bzjkkppqtv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#bzjkkppqtv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#bzjkkppqtv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#bzjkkppqtv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#bzjkkppqtv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bzjkkppqtv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bzjkkppqtv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#bzjkkppqtv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#bzjkkppqtv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#bzjkkppqtv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#bzjkkppqtv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bzjkkppqtv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bzjkkppqtv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#bzjkkppqtv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bzjkkppqtv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#bzjkkppqtv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bzjkkppqtv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#bzjkkppqtv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bzjkkppqtv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bzjkkppqtv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bzjkkppqtv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bzjkkppqtv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bzjkkppqtv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#bzjkkppqtv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bzjkkppqtv .gt_left {
  text-align: left;
}
&#10;#bzjkkppqtv .gt_center {
  text-align: center;
}
&#10;#bzjkkppqtv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bzjkkppqtv .gt_font_normal {
  font-weight: normal;
}
&#10;#bzjkkppqtv .gt_font_bold {
  font-weight: bold;
}
&#10;#bzjkkppqtv .gt_font_italic {
  font-style: italic;
}
&#10;#bzjkkppqtv .gt_super {
  font-size: 65%;
}
&#10;#bzjkkppqtv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#bzjkkppqtv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bzjkkppqtv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bzjkkppqtv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bzjkkppqtv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bzjkkppqtv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bzjkkppqtv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#bzjkkppqtv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#bzjkkppqtv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_paralysis_0">daily_paralysis_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="trx_0">trx_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="missingness">missingness</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">31-0027</td>
<td headers="daily_paralysis_0" class="gt_row gt_center">NA</td>
<td headers="trx_0" class="gt_row gt_center">Available</td>
<td headers="missingness" class="gt_row gt_left">By branching logic, "daily_paralysis_0" should have a value</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0003</td>
<td headers="daily_paralysis_0" class="gt_row gt_center">NA</td>
<td headers="trx_0" class="gt_row gt_center">NA</td>
<td headers="missingness" class="gt_row gt_left">"trx_0" is also missing</td></tr>
  </tbody>
  &#10;
</table>
</div>
