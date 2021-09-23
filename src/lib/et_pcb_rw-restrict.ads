------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PCB READ AND WRITE ROUTE/VIA RESTRICT                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

--   do do:

with et_route_restrict.board;	use et_route_restrict.board;
--with et_via_restrict;			use et_via_restrict;

package et_pcb_rw.restrict is

	--section_route_restrict		: constant string	:= "[ROUTE_RESTRICT";
	--section_via_restrict		: constant string	:= "[VIA_RESTRICT";

	--keyword_via_restring_inner	: constant string := "restring_inner";
	--keyword_via_restring_outer	: constant string := "restring_outer";
	
	
-- ROUTE RESTRICT
	use pac_route_restrict_lines;
	use pac_route_restrict_arcs;
	use pac_route_restrict_circles;
	use pac_route_restrict_polygons;
	use pac_route_restrict_cutouts;
	
	
	procedure write_line (cursor : in pac_route_restrict_lines.cursor);
	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor);
	procedure write_circle (cursor : in pac_route_restrict_circles.cursor);	
	procedure write_polygon (cursor : in pac_route_restrict_polygons.cursor);
	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor);

-- VIA RESTRICT
	--procedure write_line (cursor : in pac_via_restrict_lines.cursor);
	--procedure write_arc (cursor : in pac_via_restrict_arcs.cursor);
	--procedure write_circle (cursor : in pac_via_restrict_circles.cursor);	
	--procedure write_polygon (cursor : in pac_via_restrict_polygons.cursor);
	--procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor);

	
end et_pcb_rw.restrict;
