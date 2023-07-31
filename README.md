# GE15 
<p><b>Land doesn't vote, people do - Malaysian version</b></p>
<p>Map visualisation of Malaysia's 15th General Election results, inspired by @karim_douieb's viral map of the US Presidential Election results: </p>
<p>https://twitter.com/karim_douieb/status/1181695687005745153 </p>
<p>Number of winning votes (proportional to circle sizes) are plotted for every parliament. </p>
<p>Check out https://upload.wikimedia.org/wikipedia/commons/0/06/2022_Malaysian_general_election_results_map.svg for colour legend, although a few parties' colours are changed for better contrast. Tested the map for colour-blindness. It passes all types of colour blindness test except one. </p>
<p>@karim_douieb's viral map made me realise not every metric can be adequately represented by choropleth maps. Too often, we defaulted to choropleth maps for everything. The non-overlapping proportional circles used in the OG map, called Dorling cartogram, is better suited to represent certain data like population.</p>
<h3>Data source</h3>
<ul>
<li>SPR (prepared by <a href ="https://github.com/Thevesh/analysis-election-msia">@Thevesh</a>, huge thanks!)</li>
<li><a href ="https://github.com/dosm-malaysia/kawasanku-front">DOSM (parliamentary boundary)</a></li>
</ul>
<p></p>
<h3>R Packages </h3>
<p>Thanks to the developers of these packages:</p>
<ul>
<li>tidyverse</li>
<li>sf</li>
<li>raster</li>
<li>rgdal</li>
<li>cartogram</li>
<li>tweenr</li>
<li>gganimate</li>
<li>sfheaders</li>
<li>transformr</li>
<li>extrafont</li>
</ul>
<p>This is my first animated map. Took me some time to figure out the animation packages tweenr, and then transformr. transformr is a godsend for spatial data animation. Some boundaries look jagged in animation. The static maps look fine though.</p>

