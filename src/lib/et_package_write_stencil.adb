------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE WRITE / STENCIL                           --
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
with et_file_sections;					use et_file_sections;

with et_board_geometry;					use et_board_geometry;

with et_stencil;						use et_stencil;
with et_stencil.packages;				use et_stencil.packages;

with et_file_write;						use et_file_write;


package body et_package_write_stencil is

	use pac_geometry_2;
	use pac_file_rw;
	
	use pac_stencil_lines;
	use pac_stencil_arcs;
	use pac_stencil_circles;
	use pac_stencil_zones;

	

	procedure write_stencil (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is

		procedure write_line (cursor : in pac_stencil_lines.cursor) is 
			use pac_stencil_lines;
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		
		procedure write_arc (cursor : in pac_stencil_arcs.cursor) is 
			use pac_stencil_arcs;
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_arc , FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_stencil_circles.cursor) is 
			use pac_stencil_circles;
		begin
			section_mark (section_circle, HEADER);
			write_circle (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_circle, FOOTER);
		end write_circle;

		
		procedure write_polygon (cursor : in pac_stencil_zones.cursor) is 
			use pac_stencil_zones;
		begin
			section_mark (section_zone, HEADER);
			section_mark (section_contours, HEADER);
			write_polygon_segments (element (cursor));
			section_mark (section_contours, FOOTER);
			section_mark (section_zone, FOOTER);
		end write_polygon;
		
		
	begin
		log (text => "write stencil", level => log_threshold);

		section_mark (section_stencil, HEADER);

		-- top
		section_mark (section_top, HEADER);
		iterate (packge.stencil.top.lines, write_line'access);
		iterate (packge.stencil.top.arcs, write_arc'access);
		iterate (packge.stencil.top.circles, write_circle'access);
		iterate (packge.stencil.top.zones, write_polygon'access);
		section_mark (section_top, FOOTER);
		
		-- bottom
		section_mark (section_bottom, HEADER);
		iterate (packge.stencil.bottom.lines, write_line'access);
		iterate (packge.stencil.bottom.arcs, write_arc'access);
		iterate (packge.stencil.bottom.circles, write_circle'access);
		iterate (packge.stencil.bottom.zones, write_polygon'access);
		section_mark (section_bottom, FOOTER);

		section_mark (section_stencil, FOOTER);	
		
	end write_stencil;

	
end et_package_write_stencil;
