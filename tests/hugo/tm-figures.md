---

---
# Numbered figures

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path.eps" >}}**Figure 1.** A numbered figure with just an image.{{</tmfigure>}}

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path_webformat.svg" >}}**Figure 2.** A numbered figure with just an image, using the md-alt-image tag.{{</tmfigure>}}

{{< tmfigure class="big-figure invertible" marginal-caption="true" src="some_path_webformat.svg" >}}**Figure 3.** A numbered figure with just an image, using the md-alt-image tag. The figure is wrapped in html-class invertible.{{</tmfigure>}}

{{< tmfigure class="big-figure invertible" marginal-caption="true" >}}![](some_path_webformat.svg)![](some_path_webformat.svg)

**Figure 4.** A numbered figure with two images and md-alt-image tag, with html-class invertible.{{</tmfigure>}}

# Unnumbered and named figures

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path.eps" >}}**Figure.** An unnumbered figure with just an image.{{</tmfigure>}}

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path_webformat.svg" >}}**Figure.** An unnumbered figure with just an image, using the md-alt-image tag.{{</tmfigure>}}

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path.eps" >}}**Name with $a = b$ math.** A named figure with just an image.{{</tmfigure>}}

{{< tmfigure class="big-figure" marginal-caption="true" src="some_path_webformat.svg" >}}**Name with $a = b$ math.** A named figure with just an image, using the md-alt-image tag.{{</tmfigure>}}

# Small figures

{{< tmfigure class="small-figure" src="some_path.eps" >}}**Figure 5.** A small numbered figure with an image{{</tmfigure>}}.

{{< tmfigure class="small-figure invertible" src="some_path.eps" >}}**Figure 6.** A small numbered figure with an image. The figure is wrapped in html-class invertible.{{</tmfigure>}}

{{< tmfigure class="small-figure" src="some_path.eps" >}}**Figure.** A small unnumbered figure with an image{{</tmfigure>}}.

{{< tmfigure class="small-figure invertible" src="some_path.eps" >}}**Figure.** A small unnumbered figure with an image. The figure is wrapped in html-class invertible.{{</tmfigure>}}

# Tables

{{< tmfigure >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**Table 1.** A large numbered table.{{</tmfigure>}}

{{< tmfigure >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**Table.** A large unnumbered table.{{</tmfigure>}}

{{< tmfigure class="big-figure" marginal-caption="true" >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**A table name with $a = b$ math.** A large named table.{{</tmfigure>}}

{{< tmfigure >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**Table 2.** A small numbered table{{</tmfigure>}}.

{{< tmfigure >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**Table.** A small unnumbered table{{</tmfigure>}}.

{{< tmfigure class="small-figure" >}}<table style="display: inline-table; vertical-align: middle">
  <tbody><tr>
    <td>a</td>
    <td>b</td>
  </tr></tbody>
</table>

**A table name with $a = b$ math.** A small named table{{</tmfigure>}}.
