APS Aim 7 Derived Variables Summary
================

## Streamlined DAG: Neuromuscular Blockade

- 0 = No neuromuscular blockade on Day 0 or missing
- 1 = Neuromuscular blockade given on Day 0

### Summary

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

<div id="lnsjoakdph" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lnsjoakdph table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lnsjoakdph thead, #lnsjoakdph tbody, #lnsjoakdph tfoot, #lnsjoakdph tr, #lnsjoakdph td, #lnsjoakdph th {
  border-style: none;
}
&#10;#lnsjoakdph p {
  margin: 0;
  padding: 0;
}
&#10;#lnsjoakdph .gt_table {
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
&#10;#lnsjoakdph .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lnsjoakdph .gt_title {
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
&#10;#lnsjoakdph .gt_subtitle {
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
&#10;#lnsjoakdph .gt_heading {
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
&#10;#lnsjoakdph .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_col_headings {
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
&#10;#lnsjoakdph .gt_col_heading {
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
&#10;#lnsjoakdph .gt_column_spanner_outer {
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
&#10;#lnsjoakdph .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lnsjoakdph .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lnsjoakdph .gt_column_spanner {
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
&#10;#lnsjoakdph .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lnsjoakdph .gt_group_heading {
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
&#10;#lnsjoakdph .gt_empty_group_heading {
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
&#10;#lnsjoakdph .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lnsjoakdph .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lnsjoakdph .gt_row {
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
&#10;#lnsjoakdph .gt_stub {
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
&#10;#lnsjoakdph .gt_stub_row_group {
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
&#10;#lnsjoakdph .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lnsjoakdph .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lnsjoakdph .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnsjoakdph .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lnsjoakdph .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnsjoakdph .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lnsjoakdph .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lnsjoakdph .gt_footnotes {
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
&#10;#lnsjoakdph .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnsjoakdph .gt_sourcenotes {
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
&#10;#lnsjoakdph .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lnsjoakdph .gt_left {
  text-align: left;
}
&#10;#lnsjoakdph .gt_center {
  text-align: center;
}
&#10;#lnsjoakdph .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lnsjoakdph .gt_font_normal {
  font-weight: normal;
}
&#10;#lnsjoakdph .gt_font_bold {
  font-weight: bold;
}
&#10;#lnsjoakdph .gt_font_italic {
  font-style: italic;
}
&#10;#lnsjoakdph .gt_super {
  font-size: 65%;
}
&#10;#lnsjoakdph .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lnsjoakdph .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lnsjoakdph .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lnsjoakdph .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lnsjoakdph .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lnsjoakdph .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lnsjoakdph .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lnsjoakdph .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lnsjoakdph div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

### Missingness Report

These records were incorrectly missing `daily_paralysis_0`:

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

<div id="gwiwdexdej" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gwiwdexdej table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#gwiwdexdej thead, #gwiwdexdej tbody, #gwiwdexdej tfoot, #gwiwdexdej tr, #gwiwdexdej td, #gwiwdexdej th {
  border-style: none;
}
&#10;#gwiwdexdej p {
  margin: 0;
  padding: 0;
}
&#10;#gwiwdexdej .gt_table {
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
&#10;#gwiwdexdej .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#gwiwdexdej .gt_title {
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
&#10;#gwiwdexdej .gt_subtitle {
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
&#10;#gwiwdexdej .gt_heading {
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
&#10;#gwiwdexdej .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_col_headings {
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
&#10;#gwiwdexdej .gt_col_heading {
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
&#10;#gwiwdexdej .gt_column_spanner_outer {
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
&#10;#gwiwdexdej .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#gwiwdexdej .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#gwiwdexdej .gt_column_spanner {
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
&#10;#gwiwdexdej .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#gwiwdexdej .gt_group_heading {
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
&#10;#gwiwdexdej .gt_empty_group_heading {
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
&#10;#gwiwdexdej .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#gwiwdexdej .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#gwiwdexdej .gt_row {
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
&#10;#gwiwdexdej .gt_stub {
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
&#10;#gwiwdexdej .gt_stub_row_group {
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
&#10;#gwiwdexdej .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#gwiwdexdej .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#gwiwdexdej .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gwiwdexdej .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#gwiwdexdej .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gwiwdexdej .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#gwiwdexdej .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gwiwdexdej .gt_footnotes {
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
&#10;#gwiwdexdej .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gwiwdexdej .gt_sourcenotes {
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
&#10;#gwiwdexdej .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gwiwdexdej .gt_left {
  text-align: left;
}
&#10;#gwiwdexdej .gt_center {
  text-align: center;
}
&#10;#gwiwdexdej .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#gwiwdexdej .gt_font_normal {
  font-weight: normal;
}
&#10;#gwiwdexdej .gt_font_bold {
  font-weight: bold;
}
&#10;#gwiwdexdej .gt_font_italic {
  font-style: italic;
}
&#10;#gwiwdexdej .gt_super {
  font-size: 65%;
}
&#10;#gwiwdexdej .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#gwiwdexdej .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#gwiwdexdej .gt_indent_1 {
  text-indent: 5px;
}
&#10;#gwiwdexdej .gt_indent_2 {
  text-indent: 10px;
}
&#10;#gwiwdexdej .gt_indent_3 {
  text-indent: 15px;
}
&#10;#gwiwdexdej .gt_indent_4 {
  text-indent: 20px;
}
&#10;#gwiwdexdej .gt_indent_5 {
  text-indent: 25px;
}
&#10;#gwiwdexdej .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#gwiwdexdej div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

#### Valid NA Values

These records were missing `daily_paralysis_0`, but this was considered
valid by the branching logic of `trx_0 == 'Not Available'`. Thus, they
were set to `str_nmblockade_0 = 0` according to the variable definition.
However, I include them here since I don’t understand what `trx_0`
represents.

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_paralysis_0) & (trx_0 == 'Not Available')) |>
  select(record_id, daily_paralysis_0, trx_0) |>
  gt()
```

<div id="ytzepjxcwn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ytzepjxcwn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ytzepjxcwn thead, #ytzepjxcwn tbody, #ytzepjxcwn tfoot, #ytzepjxcwn tr, #ytzepjxcwn td, #ytzepjxcwn th {
  border-style: none;
}
&#10;#ytzepjxcwn p {
  margin: 0;
  padding: 0;
}
&#10;#ytzepjxcwn .gt_table {
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
&#10;#ytzepjxcwn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ytzepjxcwn .gt_title {
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
&#10;#ytzepjxcwn .gt_subtitle {
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
&#10;#ytzepjxcwn .gt_heading {
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
&#10;#ytzepjxcwn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_col_headings {
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
&#10;#ytzepjxcwn .gt_col_heading {
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
&#10;#ytzepjxcwn .gt_column_spanner_outer {
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
&#10;#ytzepjxcwn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ytzepjxcwn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ytzepjxcwn .gt_column_spanner {
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
&#10;#ytzepjxcwn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ytzepjxcwn .gt_group_heading {
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
&#10;#ytzepjxcwn .gt_empty_group_heading {
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
&#10;#ytzepjxcwn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ytzepjxcwn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ytzepjxcwn .gt_row {
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
&#10;#ytzepjxcwn .gt_stub {
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
&#10;#ytzepjxcwn .gt_stub_row_group {
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
&#10;#ytzepjxcwn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ytzepjxcwn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ytzepjxcwn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytzepjxcwn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ytzepjxcwn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytzepjxcwn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ytzepjxcwn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ytzepjxcwn .gt_footnotes {
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
&#10;#ytzepjxcwn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytzepjxcwn .gt_sourcenotes {
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
&#10;#ytzepjxcwn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ytzepjxcwn .gt_left {
  text-align: left;
}
&#10;#ytzepjxcwn .gt_center {
  text-align: center;
}
&#10;#ytzepjxcwn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ytzepjxcwn .gt_font_normal {
  font-weight: normal;
}
&#10;#ytzepjxcwn .gt_font_bold {
  font-weight: bold;
}
&#10;#ytzepjxcwn .gt_font_italic {
  font-style: italic;
}
&#10;#ytzepjxcwn .gt_super {
  font-size: 65%;
}
&#10;#ytzepjxcwn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ytzepjxcwn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ytzepjxcwn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ytzepjxcwn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ytzepjxcwn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ytzepjxcwn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ytzepjxcwn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ytzepjxcwn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ytzepjxcwn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

## Streamlined DAG: Inflammatory Profile

- 0 = Day 0 CRP not checked
- 1 = Day 0 CRP checked and \< 15
- 2 = Day 0 CRP checked and ≥ 15

### Summary

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate(str_inflamprofile_0 = calc_str_inflamprofile_0(
    daily_crp_8a_0 = daily_crp_8a_0,
    daily_crp_nc_0 = daily_crp_nc_0
  )) |>
  count(str_inflamprofile_0) |>
  gt()
```

<div id="ttzgprvbeq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ttzgprvbeq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ttzgprvbeq thead, #ttzgprvbeq tbody, #ttzgprvbeq tfoot, #ttzgprvbeq tr, #ttzgprvbeq td, #ttzgprvbeq th {
  border-style: none;
}
&#10;#ttzgprvbeq p {
  margin: 0;
  padding: 0;
}
&#10;#ttzgprvbeq .gt_table {
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
&#10;#ttzgprvbeq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ttzgprvbeq .gt_title {
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
&#10;#ttzgprvbeq .gt_subtitle {
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
&#10;#ttzgprvbeq .gt_heading {
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
&#10;#ttzgprvbeq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_col_headings {
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
&#10;#ttzgprvbeq .gt_col_heading {
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
&#10;#ttzgprvbeq .gt_column_spanner_outer {
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
&#10;#ttzgprvbeq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ttzgprvbeq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ttzgprvbeq .gt_column_spanner {
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
&#10;#ttzgprvbeq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ttzgprvbeq .gt_group_heading {
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
&#10;#ttzgprvbeq .gt_empty_group_heading {
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
&#10;#ttzgprvbeq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ttzgprvbeq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ttzgprvbeq .gt_row {
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
&#10;#ttzgprvbeq .gt_stub {
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
&#10;#ttzgprvbeq .gt_stub_row_group {
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
&#10;#ttzgprvbeq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ttzgprvbeq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ttzgprvbeq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ttzgprvbeq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ttzgprvbeq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ttzgprvbeq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ttzgprvbeq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ttzgprvbeq .gt_footnotes {
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
&#10;#ttzgprvbeq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ttzgprvbeq .gt_sourcenotes {
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
&#10;#ttzgprvbeq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ttzgprvbeq .gt_left {
  text-align: left;
}
&#10;#ttzgprvbeq .gt_center {
  text-align: center;
}
&#10;#ttzgprvbeq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ttzgprvbeq .gt_font_normal {
  font-weight: normal;
}
&#10;#ttzgprvbeq .gt_font_bold {
  font-weight: bold;
}
&#10;#ttzgprvbeq .gt_font_italic {
  font-style: italic;
}
&#10;#ttzgprvbeq .gt_super {
  font-size: 65%;
}
&#10;#ttzgprvbeq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ttzgprvbeq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ttzgprvbeq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ttzgprvbeq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ttzgprvbeq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ttzgprvbeq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ttzgprvbeq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ttzgprvbeq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ttzgprvbeq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="str_inflamprofile_0">str_inflamprofile_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="str_inflamprofile_0" class="gt_row gt_right">0</td>
<td headers="n" class="gt_row gt_right">461</td></tr>
    <tr><td headers="str_inflamprofile_0" class="gt_row gt_right">1</td>
<td headers="n" class="gt_row gt_right">6</td></tr>
    <tr><td headers="str_inflamprofile_0" class="gt_row gt_right">2</td>
<td headers="n" class="gt_row gt_right">29</td></tr>
    <tr><td headers="str_inflamprofile_0" class="gt_row gt_right">NA</td>
<td headers="n" class="gt_row gt_right">3</td></tr>
  </tbody>
  &#10;  
</table>
</div>

### Missingness Report

These records were incorrectly missing both `daily_crp_8a_0` and
`daily_crp_nc_0`:

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_crp_8a_0) & is.na(daily_crp_nc_0)) |>
  select(record_id, daily_crp_8a_0, daily_crp_nc_0) |>
  gt()
```

<div id="jceltofpvx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jceltofpvx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jceltofpvx thead, #jceltofpvx tbody, #jceltofpvx tfoot, #jceltofpvx tr, #jceltofpvx td, #jceltofpvx th {
  border-style: none;
}
&#10;#jceltofpvx p {
  margin: 0;
  padding: 0;
}
&#10;#jceltofpvx .gt_table {
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
&#10;#jceltofpvx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jceltofpvx .gt_title {
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
&#10;#jceltofpvx .gt_subtitle {
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
&#10;#jceltofpvx .gt_heading {
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
&#10;#jceltofpvx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_col_headings {
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
&#10;#jceltofpvx .gt_col_heading {
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
&#10;#jceltofpvx .gt_column_spanner_outer {
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
&#10;#jceltofpvx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jceltofpvx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jceltofpvx .gt_column_spanner {
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
&#10;#jceltofpvx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jceltofpvx .gt_group_heading {
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
&#10;#jceltofpvx .gt_empty_group_heading {
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
&#10;#jceltofpvx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jceltofpvx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jceltofpvx .gt_row {
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
&#10;#jceltofpvx .gt_stub {
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
&#10;#jceltofpvx .gt_stub_row_group {
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
&#10;#jceltofpvx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jceltofpvx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jceltofpvx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jceltofpvx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jceltofpvx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jceltofpvx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jceltofpvx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jceltofpvx .gt_footnotes {
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
&#10;#jceltofpvx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jceltofpvx .gt_sourcenotes {
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
&#10;#jceltofpvx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jceltofpvx .gt_left {
  text-align: left;
}
&#10;#jceltofpvx .gt_center {
  text-align: center;
}
&#10;#jceltofpvx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jceltofpvx .gt_font_normal {
  font-weight: normal;
}
&#10;#jceltofpvx .gt_font_bold {
  font-weight: bold;
}
&#10;#jceltofpvx .gt_font_italic {
  font-style: italic;
}
&#10;#jceltofpvx .gt_super {
  font-size: 65%;
}
&#10;#jceltofpvx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jceltofpvx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jceltofpvx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jceltofpvx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jceltofpvx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jceltofpvx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jceltofpvx .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jceltofpvx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jceltofpvx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="daily_crp_8a_0">daily_crp_8a_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_crp_nc_0">daily_crp_nc_0</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td>
<td headers="daily_crp_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_crp_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td>
<td headers="daily_crp_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_crp_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td>
<td headers="daily_crp_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_crp_nc_0" class="gt_row gt_center">NA</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## Systematic DAG: Inflammatory Profile

Four variables, one for each element:

1.  `crp_0`

- 0 = Day 0 CRP not checked
- 1 = Day 0 CRP checked and \< 15
- 2 = Day 0 CRP checked and ≥ 15

2.  `ferritin_0`

- 0 = Day 0 Ferritin not checked
- 1 = Day 0 Ferritin checked and \< 700
- 2 = Day 0 Ferritin checked and ≥ 700

3.  `fibrinogen_0`

- 0 = Day 0 Fibrinogen not checked
- 1 = Day 0 Fibrinogen checked and \< 150
- 2 = Day 0 Fibrinogen checked and ≥ 150

4.  `ddimer_0`

- 0 = Day 0 D-dimer not checked
- 1 = Day 0 D-dimer checked and \< 1500
- 2 = Day 0 D-dimer checked and ≥ 1500

### Summary

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate(crp_0 = calc_crp_0(daily_crp_8a_0, daily_crp_nc_0)) |>
  mutate(ferritin_0 = calc_ferritin_0(daily_ferritin_8a_0, daily_ferritin_nc_0)) |>
  mutate(fibrinogen_0 = calc_fibrinogen_0(daily_fibrinogen_8a_0, daily_fibrinogen_nc_0)) |>
  mutate(ddimer_0 = calc_ddimer_0(daily_ddimer_8a_0, daily_ddimer_nc_0)) |>
  pivot_longer(
    cols = c(crp_0, ferritin_0, fibrinogen_0, ddimer_0),
    names_to = "variable",
    values_to = "level"
  ) |>
  count(variable, level) |>
  pivot_wider(
    names_from = level,
    values_from = n
  ) |>
  gt()
```

<div id="pglkgcqcok" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pglkgcqcok table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pglkgcqcok thead, #pglkgcqcok tbody, #pglkgcqcok tfoot, #pglkgcqcok tr, #pglkgcqcok td, #pglkgcqcok th {
  border-style: none;
}
&#10;#pglkgcqcok p {
  margin: 0;
  padding: 0;
}
&#10;#pglkgcqcok .gt_table {
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
&#10;#pglkgcqcok .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pglkgcqcok .gt_title {
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
&#10;#pglkgcqcok .gt_subtitle {
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
&#10;#pglkgcqcok .gt_heading {
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
&#10;#pglkgcqcok .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_col_headings {
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
&#10;#pglkgcqcok .gt_col_heading {
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
&#10;#pglkgcqcok .gt_column_spanner_outer {
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
&#10;#pglkgcqcok .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pglkgcqcok .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pglkgcqcok .gt_column_spanner {
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
&#10;#pglkgcqcok .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pglkgcqcok .gt_group_heading {
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
&#10;#pglkgcqcok .gt_empty_group_heading {
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
&#10;#pglkgcqcok .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pglkgcqcok .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pglkgcqcok .gt_row {
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
&#10;#pglkgcqcok .gt_stub {
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
&#10;#pglkgcqcok .gt_stub_row_group {
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
&#10;#pglkgcqcok .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pglkgcqcok .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pglkgcqcok .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pglkgcqcok .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pglkgcqcok .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pglkgcqcok .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pglkgcqcok .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pglkgcqcok .gt_footnotes {
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
&#10;#pglkgcqcok .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pglkgcqcok .gt_sourcenotes {
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
&#10;#pglkgcqcok .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pglkgcqcok .gt_left {
  text-align: left;
}
&#10;#pglkgcqcok .gt_center {
  text-align: center;
}
&#10;#pglkgcqcok .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pglkgcqcok .gt_font_normal {
  font-weight: normal;
}
&#10;#pglkgcqcok .gt_font_bold {
  font-weight: bold;
}
&#10;#pglkgcqcok .gt_font_italic {
  font-style: italic;
}
&#10;#pglkgcqcok .gt_super {
  font-size: 65%;
}
&#10;#pglkgcqcok .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pglkgcqcok .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pglkgcqcok .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pglkgcqcok .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pglkgcqcok .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pglkgcqcok .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pglkgcqcok .gt_indent_5 {
  text-indent: 25px;
}
&#10;#pglkgcqcok .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#pglkgcqcok div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="variable">variable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a0">0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a1">1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a2">2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="NA">NA</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="variable" class="gt_row gt_left">crp_0</td>
<td headers="0" class="gt_row gt_right">461</td>
<td headers="1" class="gt_row gt_right">6</td>
<td headers="2" class="gt_row gt_right">29</td>
<td headers="NA" class="gt_row gt_right">3</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">ddimer_0</td>
<td headers="0" class="gt_row gt_right">464</td>
<td headers="1" class="gt_row gt_right">29</td>
<td headers="2" class="gt_row gt_right">3</td>
<td headers="NA" class="gt_row gt_right">3</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">ferritin_0</td>
<td headers="0" class="gt_row gt_right">459</td>
<td headers="1" class="gt_row gt_right">20</td>
<td headers="2" class="gt_row gt_right">17</td>
<td headers="NA" class="gt_row gt_right">3</td></tr>
    <tr><td headers="variable" class="gt_row gt_left">fibrinogen_0</td>
<td headers="0" class="gt_row gt_right">431</td>
<td headers="1" class="gt_row gt_right">8</td>
<td headers="2" class="gt_row gt_right">56</td>
<td headers="NA" class="gt_row gt_right">4</td></tr>
  </tbody>
  &#10;  
</table>
</div>

### Missingness Report

For the `crp_0` variable, see the invalid NA values for the streamlined
DAG inflammatory profile variable.

These records were incorrectly missing both `daily_ferritin_8a_0` and
`daily_ferritin_nc_0`:

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_ferritin_8a_0) & is.na(daily_ferritin_nc_0)) |>
  select(record_id, daily_ferritin_8a_0, daily_ferritin_nc_0) |>
  gt()
```

<div id="ggovanqqws" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ggovanqqws table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ggovanqqws thead, #ggovanqqws tbody, #ggovanqqws tfoot, #ggovanqqws tr, #ggovanqqws td, #ggovanqqws th {
  border-style: none;
}
&#10;#ggovanqqws p {
  margin: 0;
  padding: 0;
}
&#10;#ggovanqqws .gt_table {
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
&#10;#ggovanqqws .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ggovanqqws .gt_title {
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
&#10;#ggovanqqws .gt_subtitle {
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
&#10;#ggovanqqws .gt_heading {
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
&#10;#ggovanqqws .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_col_headings {
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
&#10;#ggovanqqws .gt_col_heading {
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
&#10;#ggovanqqws .gt_column_spanner_outer {
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
&#10;#ggovanqqws .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ggovanqqws .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ggovanqqws .gt_column_spanner {
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
&#10;#ggovanqqws .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ggovanqqws .gt_group_heading {
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
&#10;#ggovanqqws .gt_empty_group_heading {
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
&#10;#ggovanqqws .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ggovanqqws .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ggovanqqws .gt_row {
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
&#10;#ggovanqqws .gt_stub {
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
&#10;#ggovanqqws .gt_stub_row_group {
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
&#10;#ggovanqqws .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ggovanqqws .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ggovanqqws .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ggovanqqws .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ggovanqqws .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ggovanqqws .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ggovanqqws .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ggovanqqws .gt_footnotes {
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
&#10;#ggovanqqws .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ggovanqqws .gt_sourcenotes {
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
&#10;#ggovanqqws .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ggovanqqws .gt_left {
  text-align: left;
}
&#10;#ggovanqqws .gt_center {
  text-align: center;
}
&#10;#ggovanqqws .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ggovanqqws .gt_font_normal {
  font-weight: normal;
}
&#10;#ggovanqqws .gt_font_bold {
  font-weight: bold;
}
&#10;#ggovanqqws .gt_font_italic {
  font-style: italic;
}
&#10;#ggovanqqws .gt_super {
  font-size: 65%;
}
&#10;#ggovanqqws .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ggovanqqws .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ggovanqqws .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ggovanqqws .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ggovanqqws .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ggovanqqws .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ggovanqqws .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ggovanqqws .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ggovanqqws div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="daily_ferritin_8a_0">daily_ferritin_8a_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_ferritin_nc_0">daily_ferritin_nc_0</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td>
<td headers="daily_ferritin_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ferritin_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td>
<td headers="daily_ferritin_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ferritin_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td>
<td headers="daily_ferritin_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ferritin_nc_0" class="gt_row gt_center">NA</td></tr>
  </tbody>
  &#10;  
</table>
</div>

These records were incorrectly missing both `daily_fibrinogen_8a_0` and
`daily_fibrinogen_nc_0`:

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_fibrinogen_8a_0) & is.na(daily_fibrinogen_nc_0)) |>
  select(record_id, daily_fibrinogen_8a_0, daily_fibrinogen_nc_0) |>
  gt()
```

<div id="ylxgwseiow" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ylxgwseiow table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ylxgwseiow thead, #ylxgwseiow tbody, #ylxgwseiow tfoot, #ylxgwseiow tr, #ylxgwseiow td, #ylxgwseiow th {
  border-style: none;
}
&#10;#ylxgwseiow p {
  margin: 0;
  padding: 0;
}
&#10;#ylxgwseiow .gt_table {
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
&#10;#ylxgwseiow .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ylxgwseiow .gt_title {
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
&#10;#ylxgwseiow .gt_subtitle {
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
&#10;#ylxgwseiow .gt_heading {
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
&#10;#ylxgwseiow .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_col_headings {
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
&#10;#ylxgwseiow .gt_col_heading {
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
&#10;#ylxgwseiow .gt_column_spanner_outer {
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
&#10;#ylxgwseiow .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ylxgwseiow .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ylxgwseiow .gt_column_spanner {
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
&#10;#ylxgwseiow .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ylxgwseiow .gt_group_heading {
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
&#10;#ylxgwseiow .gt_empty_group_heading {
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
&#10;#ylxgwseiow .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ylxgwseiow .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ylxgwseiow .gt_row {
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
&#10;#ylxgwseiow .gt_stub {
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
&#10;#ylxgwseiow .gt_stub_row_group {
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
&#10;#ylxgwseiow .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ylxgwseiow .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ylxgwseiow .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylxgwseiow .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ylxgwseiow .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylxgwseiow .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ylxgwseiow .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ylxgwseiow .gt_footnotes {
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
&#10;#ylxgwseiow .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylxgwseiow .gt_sourcenotes {
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
&#10;#ylxgwseiow .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ylxgwseiow .gt_left {
  text-align: left;
}
&#10;#ylxgwseiow .gt_center {
  text-align: center;
}
&#10;#ylxgwseiow .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ylxgwseiow .gt_font_normal {
  font-weight: normal;
}
&#10;#ylxgwseiow .gt_font_bold {
  font-weight: bold;
}
&#10;#ylxgwseiow .gt_font_italic {
  font-style: italic;
}
&#10;#ylxgwseiow .gt_super {
  font-size: 65%;
}
&#10;#ylxgwseiow .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ylxgwseiow .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ylxgwseiow .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ylxgwseiow .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ylxgwseiow .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ylxgwseiow .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ylxgwseiow .gt_indent_5 {
  text-indent: 25px;
}
&#10;#ylxgwseiow .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#ylxgwseiow div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="daily_fibrinogen_8a_0">daily_fibrinogen_8a_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_fibrinogen_nc_0">daily_fibrinogen_nc_0</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td>
<td headers="daily_fibrinogen_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_fibrinogen_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0019</td>
<td headers="daily_fibrinogen_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_fibrinogen_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td>
<td headers="daily_fibrinogen_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_fibrinogen_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td>
<td headers="daily_fibrinogen_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_fibrinogen_nc_0" class="gt_row gt_center">NA</td></tr>
  </tbody>
  &#10;  
</table>
</div>

These records were incorrectly missing both `daily_ddimer_8a_0` and
`daily_ddimer_nc_0`:

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_ddimer_8a_0) & is.na(daily_ddimer_nc_0)) |>
  select(record_id, daily_ddimer_8a_0, daily_ddimer_nc_0) |>
  gt()
```

<div id="slcjxbbmiz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#slcjxbbmiz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#slcjxbbmiz thead, #slcjxbbmiz tbody, #slcjxbbmiz tfoot, #slcjxbbmiz tr, #slcjxbbmiz td, #slcjxbbmiz th {
  border-style: none;
}
&#10;#slcjxbbmiz p {
  margin: 0;
  padding: 0;
}
&#10;#slcjxbbmiz .gt_table {
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
&#10;#slcjxbbmiz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#slcjxbbmiz .gt_title {
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
&#10;#slcjxbbmiz .gt_subtitle {
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
&#10;#slcjxbbmiz .gt_heading {
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
&#10;#slcjxbbmiz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_col_headings {
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
&#10;#slcjxbbmiz .gt_col_heading {
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
&#10;#slcjxbbmiz .gt_column_spanner_outer {
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
&#10;#slcjxbbmiz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#slcjxbbmiz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#slcjxbbmiz .gt_column_spanner {
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
&#10;#slcjxbbmiz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#slcjxbbmiz .gt_group_heading {
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
&#10;#slcjxbbmiz .gt_empty_group_heading {
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
&#10;#slcjxbbmiz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#slcjxbbmiz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#slcjxbbmiz .gt_row {
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
&#10;#slcjxbbmiz .gt_stub {
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
&#10;#slcjxbbmiz .gt_stub_row_group {
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
&#10;#slcjxbbmiz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#slcjxbbmiz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#slcjxbbmiz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#slcjxbbmiz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#slcjxbbmiz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#slcjxbbmiz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#slcjxbbmiz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#slcjxbbmiz .gt_footnotes {
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
&#10;#slcjxbbmiz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#slcjxbbmiz .gt_sourcenotes {
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
&#10;#slcjxbbmiz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#slcjxbbmiz .gt_left {
  text-align: left;
}
&#10;#slcjxbbmiz .gt_center {
  text-align: center;
}
&#10;#slcjxbbmiz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#slcjxbbmiz .gt_font_normal {
  font-weight: normal;
}
&#10;#slcjxbbmiz .gt_font_bold {
  font-weight: bold;
}
&#10;#slcjxbbmiz .gt_font_italic {
  font-style: italic;
}
&#10;#slcjxbbmiz .gt_super {
  font-size: 65%;
}
&#10;#slcjxbbmiz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#slcjxbbmiz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#slcjxbbmiz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#slcjxbbmiz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#slcjxbbmiz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#slcjxbbmiz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#slcjxbbmiz .gt_indent_5 {
  text-indent: 25px;
}
&#10;#slcjxbbmiz .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#slcjxbbmiz div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="daily_ddimer_8a_0">daily_ddimer_8a_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_ddimer_nc_0">daily_ddimer_nc_0</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td>
<td headers="daily_ddimer_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ddimer_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td>
<td headers="daily_ddimer_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ddimer_nc_0" class="gt_row gt_center">NA</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td>
<td headers="daily_ddimer_8a_0" class="gt_row gt_right">NA</td>
<td headers="daily_ddimer_nc_0" class="gt_row gt_center">NA</td></tr>
  </tbody>
  &#10;  
</table>
</div>

<!--
## DAG: 
&#10;### Summary
&#10;
``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate() |>  # Add your calculation here
  count() |>    # Add your grouping variable here
  gt()
```
&#10;
```{=html}
<div id="mwsqkpuvbv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#mwsqkpuvbv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#mwsqkpuvbv thead, #mwsqkpuvbv tbody, #mwsqkpuvbv tfoot, #mwsqkpuvbv tr, #mwsqkpuvbv td, #mwsqkpuvbv th {
  border-style: none;
}
&#10;#mwsqkpuvbv p {
  margin: 0;
  padding: 0;
}
&#10;#mwsqkpuvbv .gt_table {
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
&#10;#mwsqkpuvbv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#mwsqkpuvbv .gt_title {
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
&#10;#mwsqkpuvbv .gt_subtitle {
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
&#10;#mwsqkpuvbv .gt_heading {
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
&#10;#mwsqkpuvbv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_col_headings {
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
&#10;#mwsqkpuvbv .gt_col_heading {
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
&#10;#mwsqkpuvbv .gt_column_spanner_outer {
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
&#10;#mwsqkpuvbv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#mwsqkpuvbv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#mwsqkpuvbv .gt_column_spanner {
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
&#10;#mwsqkpuvbv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#mwsqkpuvbv .gt_group_heading {
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
&#10;#mwsqkpuvbv .gt_empty_group_heading {
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
&#10;#mwsqkpuvbv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#mwsqkpuvbv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#mwsqkpuvbv .gt_row {
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
&#10;#mwsqkpuvbv .gt_stub {
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
&#10;#mwsqkpuvbv .gt_stub_row_group {
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
&#10;#mwsqkpuvbv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#mwsqkpuvbv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#mwsqkpuvbv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mwsqkpuvbv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#mwsqkpuvbv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mwsqkpuvbv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#mwsqkpuvbv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#mwsqkpuvbv .gt_footnotes {
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
&#10;#mwsqkpuvbv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mwsqkpuvbv .gt_sourcenotes {
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
&#10;#mwsqkpuvbv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#mwsqkpuvbv .gt_left {
  text-align: left;
}
&#10;#mwsqkpuvbv .gt_center {
  text-align: center;
}
&#10;#mwsqkpuvbv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#mwsqkpuvbv .gt_font_normal {
  font-weight: normal;
}
&#10;#mwsqkpuvbv .gt_font_bold {
  font-weight: bold;
}
&#10;#mwsqkpuvbv .gt_font_italic {
  font-style: italic;
}
&#10;#mwsqkpuvbv .gt_super {
  font-size: 65%;
}
&#10;#mwsqkpuvbv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#mwsqkpuvbv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#mwsqkpuvbv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#mwsqkpuvbv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#mwsqkpuvbv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#mwsqkpuvbv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#mwsqkpuvbv .gt_indent_5 {
  text-indent: 25px;
}
&#10;#mwsqkpuvbv .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#mwsqkpuvbv div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="n" class="gt_row gt_right">499</td></tr>
  </tbody>
  &#10;  
</table>
</div>
```
&#10;
### Missingness Report
&#10;#### Valid NA Values
&#10;
``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter() |>  # Add your NA validation conditions here
  select(record_id) |>  # Add your relevant columns here
  gt()
```
&#10;
```{=html}
<div id="aylpoikmok" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#aylpoikmok table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#aylpoikmok thead, #aylpoikmok tbody, #aylpoikmok tfoot, #aylpoikmok tr, #aylpoikmok td, #aylpoikmok th {
  border-style: none;
}
&#10;#aylpoikmok p {
  margin: 0;
  padding: 0;
}
&#10;#aylpoikmok .gt_table {
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
&#10;#aylpoikmok .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#aylpoikmok .gt_title {
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
&#10;#aylpoikmok .gt_subtitle {
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
&#10;#aylpoikmok .gt_heading {
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
&#10;#aylpoikmok .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_col_headings {
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
&#10;#aylpoikmok .gt_col_heading {
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
&#10;#aylpoikmok .gt_column_spanner_outer {
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
&#10;#aylpoikmok .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#aylpoikmok .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#aylpoikmok .gt_column_spanner {
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
&#10;#aylpoikmok .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#aylpoikmok .gt_group_heading {
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
&#10;#aylpoikmok .gt_empty_group_heading {
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
&#10;#aylpoikmok .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#aylpoikmok .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#aylpoikmok .gt_row {
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
&#10;#aylpoikmok .gt_stub {
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
&#10;#aylpoikmok .gt_stub_row_group {
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
&#10;#aylpoikmok .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#aylpoikmok .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#aylpoikmok .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aylpoikmok .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#aylpoikmok .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aylpoikmok .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#aylpoikmok .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#aylpoikmok .gt_footnotes {
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
&#10;#aylpoikmok .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aylpoikmok .gt_sourcenotes {
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
&#10;#aylpoikmok .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#aylpoikmok .gt_left {
  text-align: left;
}
&#10;#aylpoikmok .gt_center {
  text-align: center;
}
&#10;#aylpoikmok .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#aylpoikmok .gt_font_normal {
  font-weight: normal;
}
&#10;#aylpoikmok .gt_font_bold {
  font-weight: bold;
}
&#10;#aylpoikmok .gt_font_italic {
  font-style: italic;
}
&#10;#aylpoikmok .gt_super {
  font-size: 65%;
}
&#10;#aylpoikmok .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#aylpoikmok .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#aylpoikmok .gt_indent_1 {
  text-indent: 5px;
}
&#10;#aylpoikmok .gt_indent_2 {
  text-indent: 10px;
}
&#10;#aylpoikmok .gt_indent_3 {
  text-indent: 15px;
}
&#10;#aylpoikmok .gt_indent_4 {
  text-indent: 20px;
}
&#10;#aylpoikmok .gt_indent_5 {
  text-indent: 25px;
}
&#10;#aylpoikmok .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#aylpoikmok div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">11-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0054</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0060</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0062</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0064</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0066</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0069</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0070</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0075</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0076</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0077</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0049</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0053</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0055</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0057</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0062</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0063</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0064</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0049</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0053</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0054</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0055</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0057</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0060</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0063</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0066</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0070</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0072</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0018</td></tr>
  </tbody>
  &#10;  
</table>
</div>
```
&#10;
#### Invalid NA Values
&#10;
``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter() |>  # Add your invalid NA conditions here
  select(record_id) |>  # Add your relevant columns here
  gt()
```
&#10;
```{=html}
<div id="kbyksolbqt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#kbyksolbqt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#kbyksolbqt thead, #kbyksolbqt tbody, #kbyksolbqt tfoot, #kbyksolbqt tr, #kbyksolbqt td, #kbyksolbqt th {
  border-style: none;
}
&#10;#kbyksolbqt p {
  margin: 0;
  padding: 0;
}
&#10;#kbyksolbqt .gt_table {
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
&#10;#kbyksolbqt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#kbyksolbqt .gt_title {
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
&#10;#kbyksolbqt .gt_subtitle {
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
&#10;#kbyksolbqt .gt_heading {
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
&#10;#kbyksolbqt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_col_headings {
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
&#10;#kbyksolbqt .gt_col_heading {
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
&#10;#kbyksolbqt .gt_column_spanner_outer {
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
&#10;#kbyksolbqt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#kbyksolbqt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#kbyksolbqt .gt_column_spanner {
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
&#10;#kbyksolbqt .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#kbyksolbqt .gt_group_heading {
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
&#10;#kbyksolbqt .gt_empty_group_heading {
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
&#10;#kbyksolbqt .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#kbyksolbqt .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#kbyksolbqt .gt_row {
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
&#10;#kbyksolbqt .gt_stub {
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
&#10;#kbyksolbqt .gt_stub_row_group {
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
&#10;#kbyksolbqt .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#kbyksolbqt .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#kbyksolbqt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kbyksolbqt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#kbyksolbqt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kbyksolbqt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#kbyksolbqt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#kbyksolbqt .gt_footnotes {
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
&#10;#kbyksolbqt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kbyksolbqt .gt_sourcenotes {
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
&#10;#kbyksolbqt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#kbyksolbqt .gt_left {
  text-align: left;
}
&#10;#kbyksolbqt .gt_center {
  text-align: center;
}
&#10;#kbyksolbqt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#kbyksolbqt .gt_font_normal {
  font-weight: normal;
}
&#10;#kbyksolbqt .gt_font_bold {
  font-weight: bold;
}
&#10;#kbyksolbqt .gt_font_italic {
  font-style: italic;
}
&#10;#kbyksolbqt .gt_super {
  font-size: 65%;
}
&#10;#kbyksolbqt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#kbyksolbqt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#kbyksolbqt .gt_indent_1 {
  text-indent: 5px;
}
&#10;#kbyksolbqt .gt_indent_2 {
  text-indent: 10px;
}
&#10;#kbyksolbqt .gt_indent_3 {
  text-indent: 15px;
}
&#10;#kbyksolbqt .gt_indent_4 {
  text-indent: 20px;
}
&#10;#kbyksolbqt .gt_indent_5 {
  text-indent: 25px;
}
&#10;#kbyksolbqt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#kbyksolbqt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="record_id">record_id</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="record_id" class="gt_row gt_right">11-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0054</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0060</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0062</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0064</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0066</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0069</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0070</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0075</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0076</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">11-0077</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">12-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">13-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">21-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">22-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">24-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0049</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0053</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0055</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0057</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0062</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0063</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0064</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">31-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">33-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">41-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">42-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">43-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">45-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">51-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">52-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">54-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">55-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0019</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0020</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0021</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0022</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0023</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0024</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0025</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0026</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0027</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0028</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0029</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0030</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0031</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0032</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0033</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0034</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0035</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0036</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0037</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0038</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0039</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0040</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0041</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0042</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0043</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0044</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0045</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0046</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0047</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0048</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0049</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0050</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0051</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0052</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0053</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0054</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0055</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0056</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0057</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0058</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0059</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0060</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0061</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0063</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0065</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0066</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0068</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0070</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">61-0072</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">63-0018</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0001</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0002</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0003</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0004</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0005</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0006</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0007</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0008</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0009</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0010</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0011</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0012</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0013</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0014</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0015</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0016</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0017</td></tr>
    <tr><td headers="record_id" class="gt_row gt_right">64-0018</td></tr>
  </tbody>
  &#10;  
</table>
</div>
```
&#10; -->
