------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PCB READ AND WRITE ROUTE/VIA RESTRICT                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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



package body et_pcb_rw.restrict is

	
-- ROUTE RESTRICT
	procedure write_line (cursor : in pac_route_restrict_lines.cursor) is 
	begin
		line_begin;
		write_line (element (cursor));
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_route_restrict_arcs.cursor) is 
	begin
		arc_begin;
		write_arc (element (cursor));		
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_route_restrict_circles.cursor) is 
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;

	
	procedure write_contour (cursor : in pac_route_restrict_contours.cursor) is 
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		fill_zone_end;
	end write_contour;

	
	procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		cutout_zone_end;
	end;



-- VIA RESTRICT
	procedure write_line (cursor : in pac_via_restrict_lines.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_lines;
	begin
		line_begin;
		write_line (element (cursor));		
		write_signal_layers (element (cursor).layers);
		line_end;
	end write_line;

	
	procedure write_arc (cursor : in pac_via_restrict_arcs.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_arcs;
	begin
		arc_begin;
		write_arc (element (cursor));
		write_signal_layers (element (cursor).layers);
		arc_end;
	end write_arc;

	
	procedure write_circle (cursor : in pac_via_restrict_circles.cursor) is 
		use et_pcb_stack;		
		use pac_via_restrict_circles;
	begin
		circle_begin;
		write_circle (element (cursor));
		write_fill_status (element (cursor).filled);
		write_signal_layers (element (cursor).layers);
		circle_end;
	end write_circle;

	
	procedure write_polygon (cursor : in pac_via_restrict_polygons.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_polygons;
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);			

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		fill_zone_end;
	end write_polygon;

	
	procedure write_cutout (cursor : in pac_via_restrict_cutouts.cursor) is 
		use pac_via_restrict_cutouts;
	begin
		cutout_zone_begin;
		write_signal_layers (element (cursor).layers);
		
		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		cutout_zone_end;
	end;

	
end et_pcb_rw.restrict;
