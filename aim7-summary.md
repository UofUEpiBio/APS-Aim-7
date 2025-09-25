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

<div id="qdgmmhfxyi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qdgmmhfxyi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qdgmmhfxyi thead, #qdgmmhfxyi tbody, #qdgmmhfxyi tfoot, #qdgmmhfxyi tr, #qdgmmhfxyi td, #qdgmmhfxyi th {
  border-style: none;
}
&#10;#qdgmmhfxyi p {
  margin: 0;
  padding: 0;
}
&#10;#qdgmmhfxyi .gt_table {
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
&#10;#qdgmmhfxyi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qdgmmhfxyi .gt_title {
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
&#10;#qdgmmhfxyi .gt_subtitle {
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
&#10;#qdgmmhfxyi .gt_heading {
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
&#10;#qdgmmhfxyi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_col_headings {
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
&#10;#qdgmmhfxyi .gt_col_heading {
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
&#10;#qdgmmhfxyi .gt_column_spanner_outer {
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
&#10;#qdgmmhfxyi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qdgmmhfxyi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qdgmmhfxyi .gt_column_spanner {
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
&#10;#qdgmmhfxyi .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qdgmmhfxyi .gt_group_heading {
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
&#10;#qdgmmhfxyi .gt_empty_group_heading {
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
&#10;#qdgmmhfxyi .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qdgmmhfxyi .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qdgmmhfxyi .gt_row {
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
&#10;#qdgmmhfxyi .gt_stub {
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
&#10;#qdgmmhfxyi .gt_stub_row_group {
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
&#10;#qdgmmhfxyi .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qdgmmhfxyi .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qdgmmhfxyi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qdgmmhfxyi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qdgmmhfxyi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qdgmmhfxyi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qdgmmhfxyi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qdgmmhfxyi .gt_footnotes {
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
&#10;#qdgmmhfxyi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qdgmmhfxyi .gt_sourcenotes {
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
&#10;#qdgmmhfxyi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qdgmmhfxyi .gt_left {
  text-align: left;
}
&#10;#qdgmmhfxyi .gt_center {
  text-align: center;
}
&#10;#qdgmmhfxyi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qdgmmhfxyi .gt_font_normal {
  font-weight: normal;
}
&#10;#qdgmmhfxyi .gt_font_bold {
  font-weight: bold;
}
&#10;#qdgmmhfxyi .gt_font_italic {
  font-style: italic;
}
&#10;#qdgmmhfxyi .gt_super {
  font-size: 65%;
}
&#10;#qdgmmhfxyi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qdgmmhfxyi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qdgmmhfxyi .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qdgmmhfxyi .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qdgmmhfxyi .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qdgmmhfxyi .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qdgmmhfxyi .gt_indent_5 {
  text-indent: 25px;
}
&#10;#qdgmmhfxyi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#qdgmmhfxyi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

#### Valid NA Values

These records were correctly missing `daily_paralysis_0` by the
branching logic of `trx_0 == 'Not Available'`. They were set to
`str_nmblockade_0 = 0` according to the variable definition.

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_paralysis_0) & (trx_0 == 'Not Available')) |>
  select(record_id, daily_paralysis_0, trx_0) |>
  gt()
```

<div id="cmzsptbfyq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#cmzsptbfyq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#cmzsptbfyq thead, #cmzsptbfyq tbody, #cmzsptbfyq tfoot, #cmzsptbfyq tr, #cmzsptbfyq td, #cmzsptbfyq th {
  border-style: none;
}
&#10;#cmzsptbfyq p {
  margin: 0;
  padding: 0;
}
&#10;#cmzsptbfyq .gt_table {
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
&#10;#cmzsptbfyq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#cmzsptbfyq .gt_title {
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
&#10;#cmzsptbfyq .gt_subtitle {
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
&#10;#cmzsptbfyq .gt_heading {
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
&#10;#cmzsptbfyq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_col_headings {
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
&#10;#cmzsptbfyq .gt_col_heading {
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
&#10;#cmzsptbfyq .gt_column_spanner_outer {
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
&#10;#cmzsptbfyq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#cmzsptbfyq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#cmzsptbfyq .gt_column_spanner {
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
&#10;#cmzsptbfyq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#cmzsptbfyq .gt_group_heading {
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
&#10;#cmzsptbfyq .gt_empty_group_heading {
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
&#10;#cmzsptbfyq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#cmzsptbfyq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#cmzsptbfyq .gt_row {
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
&#10;#cmzsptbfyq .gt_stub {
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
&#10;#cmzsptbfyq .gt_stub_row_group {
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
&#10;#cmzsptbfyq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#cmzsptbfyq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#cmzsptbfyq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cmzsptbfyq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#cmzsptbfyq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cmzsptbfyq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#cmzsptbfyq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cmzsptbfyq .gt_footnotes {
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
&#10;#cmzsptbfyq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cmzsptbfyq .gt_sourcenotes {
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
&#10;#cmzsptbfyq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cmzsptbfyq .gt_left {
  text-align: left;
}
&#10;#cmzsptbfyq .gt_center {
  text-align: center;
}
&#10;#cmzsptbfyq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#cmzsptbfyq .gt_font_normal {
  font-weight: normal;
}
&#10;#cmzsptbfyq .gt_font_bold {
  font-weight: bold;
}
&#10;#cmzsptbfyq .gt_font_italic {
  font-style: italic;
}
&#10;#cmzsptbfyq .gt_super {
  font-size: 65%;
}
&#10;#cmzsptbfyq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#cmzsptbfyq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#cmzsptbfyq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#cmzsptbfyq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#cmzsptbfyq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#cmzsptbfyq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#cmzsptbfyq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#cmzsptbfyq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#cmzsptbfyq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

<div id="pzouqphbnr" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pzouqphbnr table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#pzouqphbnr thead, #pzouqphbnr tbody, #pzouqphbnr tfoot, #pzouqphbnr tr, #pzouqphbnr td, #pzouqphbnr th {
  border-style: none;
}
&#10;#pzouqphbnr p {
  margin: 0;
  padding: 0;
}
&#10;#pzouqphbnr .gt_table {
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
&#10;#pzouqphbnr .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#pzouqphbnr .gt_title {
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
&#10;#pzouqphbnr .gt_subtitle {
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
&#10;#pzouqphbnr .gt_heading {
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
&#10;#pzouqphbnr .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_col_headings {
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
&#10;#pzouqphbnr .gt_col_heading {
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
&#10;#pzouqphbnr .gt_column_spanner_outer {
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
&#10;#pzouqphbnr .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#pzouqphbnr .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#pzouqphbnr .gt_column_spanner {
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
&#10;#pzouqphbnr .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#pzouqphbnr .gt_group_heading {
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
&#10;#pzouqphbnr .gt_empty_group_heading {
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
&#10;#pzouqphbnr .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#pzouqphbnr .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#pzouqphbnr .gt_row {
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
&#10;#pzouqphbnr .gt_stub {
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
&#10;#pzouqphbnr .gt_stub_row_group {
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
&#10;#pzouqphbnr .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#pzouqphbnr .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#pzouqphbnr .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pzouqphbnr .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#pzouqphbnr .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pzouqphbnr .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#pzouqphbnr .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#pzouqphbnr .gt_footnotes {
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
&#10;#pzouqphbnr .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pzouqphbnr .gt_sourcenotes {
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
&#10;#pzouqphbnr .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#pzouqphbnr .gt_left {
  text-align: left;
}
&#10;#pzouqphbnr .gt_center {
  text-align: center;
}
&#10;#pzouqphbnr .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#pzouqphbnr .gt_font_normal {
  font-weight: normal;
}
&#10;#pzouqphbnr .gt_font_bold {
  font-weight: bold;
}
&#10;#pzouqphbnr .gt_font_italic {
  font-style: italic;
}
&#10;#pzouqphbnr .gt_super {
  font-size: 65%;
}
&#10;#pzouqphbnr .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#pzouqphbnr .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#pzouqphbnr .gt_indent_1 {
  text-indent: 5px;
}
&#10;#pzouqphbnr .gt_indent_2 {
  text-indent: 10px;
}
&#10;#pzouqphbnr .gt_indent_3 {
  text-indent: 15px;
}
&#10;#pzouqphbnr .gt_indent_4 {
  text-indent: 20px;
}
&#10;#pzouqphbnr .gt_indent_5 {
  text-indent: 25px;
}
&#10;#pzouqphbnr .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#pzouqphbnr div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

<div id="bjnrjqnsro" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#bjnrjqnsro table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#bjnrjqnsro thead, #bjnrjqnsro tbody, #bjnrjqnsro tfoot, #bjnrjqnsro tr, #bjnrjqnsro td, #bjnrjqnsro th {
  border-style: none;
}
&#10;#bjnrjqnsro p {
  margin: 0;
  padding: 0;
}
&#10;#bjnrjqnsro .gt_table {
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
&#10;#bjnrjqnsro .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#bjnrjqnsro .gt_title {
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
&#10;#bjnrjqnsro .gt_subtitle {
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
&#10;#bjnrjqnsro .gt_heading {
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
&#10;#bjnrjqnsro .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_col_headings {
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
&#10;#bjnrjqnsro .gt_col_heading {
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
&#10;#bjnrjqnsro .gt_column_spanner_outer {
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
&#10;#bjnrjqnsro .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#bjnrjqnsro .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#bjnrjqnsro .gt_column_spanner {
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
&#10;#bjnrjqnsro .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#bjnrjqnsro .gt_group_heading {
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
&#10;#bjnrjqnsro .gt_empty_group_heading {
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
&#10;#bjnrjqnsro .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#bjnrjqnsro .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#bjnrjqnsro .gt_row {
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
&#10;#bjnrjqnsro .gt_stub {
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
&#10;#bjnrjqnsro .gt_stub_row_group {
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
&#10;#bjnrjqnsro .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#bjnrjqnsro .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#bjnrjqnsro .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bjnrjqnsro .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#bjnrjqnsro .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bjnrjqnsro .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#bjnrjqnsro .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#bjnrjqnsro .gt_footnotes {
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
&#10;#bjnrjqnsro .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bjnrjqnsro .gt_sourcenotes {
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
&#10;#bjnrjqnsro .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#bjnrjqnsro .gt_left {
  text-align: left;
}
&#10;#bjnrjqnsro .gt_center {
  text-align: center;
}
&#10;#bjnrjqnsro .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#bjnrjqnsro .gt_font_normal {
  font-weight: normal;
}
&#10;#bjnrjqnsro .gt_font_bold {
  font-weight: bold;
}
&#10;#bjnrjqnsro .gt_font_italic {
  font-style: italic;
}
&#10;#bjnrjqnsro .gt_super {
  font-size: 65%;
}
&#10;#bjnrjqnsro .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#bjnrjqnsro .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#bjnrjqnsro .gt_indent_1 {
  text-indent: 5px;
}
&#10;#bjnrjqnsro .gt_indent_2 {
  text-indent: 10px;
}
&#10;#bjnrjqnsro .gt_indent_3 {
  text-indent: 15px;
}
&#10;#bjnrjqnsro .gt_indent_4 {
  text-indent: 20px;
}
&#10;#bjnrjqnsro .gt_indent_5 {
  text-indent: 25px;
}
&#10;#bjnrjqnsro .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#bjnrjqnsro div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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

#### Valid NA Values

All missing values of `daily_crp_8a_0` expected by the branching logic
of `daily_crp_nc_0 == 'Not Collected'` were correctly set to
`str_inflamprofile_0 = 0` according to the variable definition.

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  mutate(str_inflamprofile_0 = calc_str_inflamprofile_0(
    daily_crp_8a_0 = daily_crp_8a_0,
    daily_crp_nc_0 = daily_crp_nc_0
  )) |>
  filter(is.na(daily_crp_8a_0) & (daily_crp_nc_0 == 'Not Collected')) |>
  count(daily_crp_nc_0, daily_crp_8a_0, str_inflamprofile_0) |>
  gt()
```

<div id="lejwpwylkg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#lejwpwylkg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#lejwpwylkg thead, #lejwpwylkg tbody, #lejwpwylkg tfoot, #lejwpwylkg tr, #lejwpwylkg td, #lejwpwylkg th {
  border-style: none;
}
&#10;#lejwpwylkg p {
  margin: 0;
  padding: 0;
}
&#10;#lejwpwylkg .gt_table {
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
&#10;#lejwpwylkg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#lejwpwylkg .gt_title {
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
&#10;#lejwpwylkg .gt_subtitle {
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
&#10;#lejwpwylkg .gt_heading {
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
&#10;#lejwpwylkg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_col_headings {
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
&#10;#lejwpwylkg .gt_col_heading {
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
&#10;#lejwpwylkg .gt_column_spanner_outer {
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
&#10;#lejwpwylkg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#lejwpwylkg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#lejwpwylkg .gt_column_spanner {
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
&#10;#lejwpwylkg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#lejwpwylkg .gt_group_heading {
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
&#10;#lejwpwylkg .gt_empty_group_heading {
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
&#10;#lejwpwylkg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#lejwpwylkg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#lejwpwylkg .gt_row {
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
&#10;#lejwpwylkg .gt_stub {
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
&#10;#lejwpwylkg .gt_stub_row_group {
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
&#10;#lejwpwylkg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#lejwpwylkg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#lejwpwylkg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lejwpwylkg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#lejwpwylkg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lejwpwylkg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#lejwpwylkg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#lejwpwylkg .gt_footnotes {
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
&#10;#lejwpwylkg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lejwpwylkg .gt_sourcenotes {
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
&#10;#lejwpwylkg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#lejwpwylkg .gt_left {
  text-align: left;
}
&#10;#lejwpwylkg .gt_center {
  text-align: center;
}
&#10;#lejwpwylkg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#lejwpwylkg .gt_font_normal {
  font-weight: normal;
}
&#10;#lejwpwylkg .gt_font_bold {
  font-weight: bold;
}
&#10;#lejwpwylkg .gt_font_italic {
  font-style: italic;
}
&#10;#lejwpwylkg .gt_super {
  font-size: 65%;
}
&#10;#lejwpwylkg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#lejwpwylkg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#lejwpwylkg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#lejwpwylkg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#lejwpwylkg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#lejwpwylkg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#lejwpwylkg .gt_indent_5 {
  text-indent: 25px;
}
&#10;#lejwpwylkg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#lejwpwylkg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="daily_crp_nc_0">daily_crp_nc_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="daily_crp_8a_0">daily_crp_8a_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="str_inflamprofile_0">str_inflamprofile_0</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="n">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="daily_crp_nc_0" class="gt_row gt_center">Not Collected</td>
<td headers="daily_crp_8a_0" class="gt_row gt_right">NA</td>
<td headers="str_inflamprofile_0" class="gt_row gt_right">0</td>
<td headers="n" class="gt_row gt_right">461</td></tr>
  </tbody>
  &#10;  
</table>
</div>

#### Invalid NA Values

These records were incorrectly missing both `daily_crp_8a_0` and
`daily_crp_nc_0`:

``` r
data |>
  filter(event_label == 'Daily In-Hospital Forms') |>
  filter(is.na(daily_crp_8a_0) & is.na(daily_crp_nc_0)) |>
  select(record_id, daily_crp_8a_0, daily_crp_nc_0) |>
  gt()
```

<div id="jquqwyaehn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jquqwyaehn table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jquqwyaehn thead, #jquqwyaehn tbody, #jquqwyaehn tfoot, #jquqwyaehn tr, #jquqwyaehn td, #jquqwyaehn th {
  border-style: none;
}
&#10;#jquqwyaehn p {
  margin: 0;
  padding: 0;
}
&#10;#jquqwyaehn .gt_table {
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
&#10;#jquqwyaehn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jquqwyaehn .gt_title {
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
&#10;#jquqwyaehn .gt_subtitle {
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
&#10;#jquqwyaehn .gt_heading {
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
&#10;#jquqwyaehn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_col_headings {
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
&#10;#jquqwyaehn .gt_col_heading {
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
&#10;#jquqwyaehn .gt_column_spanner_outer {
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
&#10;#jquqwyaehn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jquqwyaehn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jquqwyaehn .gt_column_spanner {
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
&#10;#jquqwyaehn .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jquqwyaehn .gt_group_heading {
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
&#10;#jquqwyaehn .gt_empty_group_heading {
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
&#10;#jquqwyaehn .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jquqwyaehn .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jquqwyaehn .gt_row {
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
&#10;#jquqwyaehn .gt_stub {
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
&#10;#jquqwyaehn .gt_stub_row_group {
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
&#10;#jquqwyaehn .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jquqwyaehn .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jquqwyaehn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jquqwyaehn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jquqwyaehn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jquqwyaehn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jquqwyaehn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jquqwyaehn .gt_footnotes {
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
&#10;#jquqwyaehn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jquqwyaehn .gt_sourcenotes {
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
&#10;#jquqwyaehn .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jquqwyaehn .gt_left {
  text-align: left;
}
&#10;#jquqwyaehn .gt_center {
  text-align: center;
}
&#10;#jquqwyaehn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jquqwyaehn .gt_font_normal {
  font-weight: normal;
}
&#10;#jquqwyaehn .gt_font_bold {
  font-weight: bold;
}
&#10;#jquqwyaehn .gt_font_italic {
  font-style: italic;
}
&#10;#jquqwyaehn .gt_super {
  font-size: 65%;
}
&#10;#jquqwyaehn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jquqwyaehn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jquqwyaehn .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jquqwyaehn .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jquqwyaehn .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jquqwyaehn .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jquqwyaehn .gt_indent_5 {
  text-indent: 25px;
}
&#10;#jquqwyaehn .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#jquqwyaehn div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
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
