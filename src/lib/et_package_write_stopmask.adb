------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE WRITE / STOPMASK                          --
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
with et_package_sections;				use et_package_sections;

with et_board_geometry;					use et_board_geometry;

with et_stopmask;						use et_stopmask;
with et_stopmask.packages;				use et_stopmask.packages;

with et_file_write;						use et_file_write;


package body et_package_write_stopmask is

	use pac_geometry_2;
	use pac_file_rw;
	
	use pac_stop_lines;
	use pac_stop_arcs;
	use pac_stop_circles;
	use pac_stop_zones;

	

	procedure write_stopmask (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is

		procedure write_line (cursor : in pac_stop_lines.cursor) is 
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		
		procedure write_arc (cursor : in pac_stop_arcs.cursor) is 
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_arc , FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_stop_circles.cursor) is 
		begin
			section_mark (section_circle, HEADER);
			write_circle (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_circle, FOOTER);
		end write_circle;

		
		procedure write_polygon (cursor : in pac_stop_zones.cursor) is 
		begin
			section_mark (section_zone, HEADER);
			section_mark (section_contours, HEADER);		
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_zone, FOOTER);
		end write_polygon;

		
		
	begin
		log (text => "write stopmask", level => log_threshold);

		section_mark (section_stopmask, HEADER);

		-- top
		section_mark (section_top, HEADER);
		iterate (packge.stop_mask.top.lines, write_line'access);
		iterate (packge.stop_mask.top.arcs, write_arc'access);
		iterate (packge.stop_mask.top.circles, write_circle'access);
		iterate (packge.stop_mask.top.zones, write_polygon'access);
		section_mark (section_top, FOOTER);
		
		-- bottom
		section_mark (section_bottom, HEADER);
		iterate (packge.stop_mask.bottom.lines, write_line'access);
		iterate (packge.stop_mask.bottom.arcs, write_arc'access);
		iterate (packge.stop_mask.bottom.circles, write_circle'access);
		iterate (packge.stop_mask.bottom.zones, write_polygon'access);			
		section_mark (section_bottom, FOOTER);

		section_mark (section_stopmask, FOOTER);			
	

	end write_stopmask;

	
end et_package_write_stopmask;
