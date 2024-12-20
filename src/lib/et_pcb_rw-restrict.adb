------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PCB READ AND WRITE ROUTE/VIA RESTRICT                  --
--                                                                          --
--                               B o d y                                    --
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

	
	procedure write_contour (cursor : in pac_via_restrict_contours.cursor) is 
		use et_pcb_stack;
		use pac_via_restrict_contours;
	begin
		fill_zone_begin;
		write_signal_layers (element (cursor).layers);			

		contours_begin;
		write_polygon_segments (element (cursor));
		contours_end;
		
		fill_zone_end;
	end write_contour;

	
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
