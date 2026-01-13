------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PACKAGE WRITE / ROUTE RESTRICT                         --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
-- To Do:
-- - clean up, use renames



with ada.text_io;				use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;

with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_package_sections;				use et_package_sections;

with et_board_geometry;					use et_board_geometry;

with et_route_restrict;					use et_route_restrict;
with et_route_restrict.packages;		use et_route_restrict.packages;

with et_general_rw;						use et_general_rw;
with et_file_write;						use et_file_write;


package body et_package_write_route_restrict is

	use pac_geometry_2;
	use pac_file_rw;
	
	use pac_route_restrict_lines;
	use pac_route_restrict_arcs;
	use pac_route_restrict_circles;
	use pac_route_restrict_zones;
	use pac_route_restrict_cutouts;

	

	procedure write_route_restrict (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is

		procedure write_line (cursor : in pac_route_restrict_lines.cursor) is 
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));
			section_mark (section_line, FOOTER);
		end write_line;

		procedure write_arc (cursor : in pac_route_restrict_arcs.cursor) is 
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));
			section_mark (section_arc , FOOTER);
		end write_arc;

		procedure write_circle (cursor : in pac_route_restrict_circles.cursor) is 
		begin
			section_mark (section_circle, HEADER);
			write_circle (element (cursor));
			section_mark (section_circle, FOOTER);
		end write_circle;
		
		procedure write_zone (cursor : in pac_route_restrict_zones.cursor) is 
		begin
			section_mark (section_zone, HEADER);
			section_mark (section_contours, HEADER);
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_zone, FOOTER);
		end write_zone;

		procedure write_cutout (cursor : in pac_route_restrict_cutouts.cursor) is 
		begin
			section_mark (section_cutout_zone, HEADER);
			section_mark (section_contours, HEADER);
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_cutout_zone, FOOTER);
		end write_cutout;
		
		
	begin
		log (text => "write route restrict", level => log_threshold);

		section_mark (section_route_restrict, HEADER);

		-- top
		section_mark (section_top, HEADER);
		iterate (packge.route_restrict.top.lines, write_line'access);
		iterate (packge.route_restrict.top.arcs, write_arc'access);
		iterate (packge.route_restrict.top.circles, write_circle'access);
		iterate (packge.route_restrict.top.zones, write_zone'access);
		iterate (packge.route_restrict.top.cutouts, write_cutout'access);
		section_mark (section_top, FOOTER);

		-- bottom
		section_mark (section_bottom, HEADER);
		iterate (packge.route_restrict.bottom.lines, write_line'access);
		iterate (packge.route_restrict.bottom.arcs, write_arc'access);
		iterate (packge.route_restrict.bottom.circles, write_circle'access);
		iterate (packge.route_restrict.bottom.zones, write_zone'access);
		iterate (packge.route_restrict.bottom.cutouts, write_cutout'access);
		section_mark (section_bottom, FOOTER);
		
		section_mark (section_route_restrict, FOOTER);			
		
	end write_route_restrict;

	
end et_package_write_route_restrict;
