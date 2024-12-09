------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PCB READ AND WRITE ROUTE/VIA RESTRICT                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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

with et_route_restrict.boards;	use et_route_restrict.boards;
with et_via_restrict.boards;	use et_via_restrict.boards;

package et_pcb_rw.restrict is

	
	
-- ROUTE RESTRICT

	use pac_route_restrict_lines;
	use pac_route_restrict_arcs;
	use pac_route_restrict_circles;
	use pac_route_restrict_contours;
	use pac_route_restrict_cutouts;
	
	procedure write_line (cursor : in pac_route_restrict_lines.cursor);
	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor);
	procedure write_circle (cursor : in pac_route_restrict_circles.cursor);	
	procedure write_contour (cursor : in pac_route_restrict_contours.cursor);
	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor);

	
-- VIA RESTRICT

	use pac_via_restrict_contours;
	use pac_via_restrict_cutouts;

	procedure write_contour (cursor : in pac_via_restrict_contours.cursor);
	procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor);

	
end et_pcb_rw.restrict;
