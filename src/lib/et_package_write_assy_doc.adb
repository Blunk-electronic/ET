------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                  PACKAGE WRITE / ASSEMBLY DOCUMENTATION                  --
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



with ada.text_io;						use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_package_sections;				use et_package_sections;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_text_content;					use et_text_content;
with et_assy_doc;						use et_assy_doc;
with et_assy_doc.packages;				use et_assy_doc.packages;

with et_board_geometry;					use et_board_geometry;
with et_general_rw;						use et_general_rw;
with et_file_write;						use et_file_write;

with et_coordinates_formatting;			use et_coordinates_formatting;
with et_alignment;						use et_alignment;


package body et_package_write_assy_doc is

	use pac_geometry_2;
	use pac_file_rw;
	
	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;
	use pac_doc_zones;
	use pac_doc_texts;
			
	use pac_text_placeholders;		

	

	procedure write_assy_doc (
		packge			: in type_package_model;
		log_threshold	: in type_log_level) 
	is

		procedure write_line (cursor : in pac_doc_lines.cursor) is 
			use pac_doc_lines;
		begin
			section_mark (section_line, HEADER);
			write_line (element (cursor));		
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_line, FOOTER);
		end write_line;

		procedure write_arc (cursor : in pac_doc_arcs.cursor) is 
			use pac_doc_arcs;
		begin
			section_mark (section_arc , HEADER);
			write_arc (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_arc , FOOTER);
		end write_arc;

		
		procedure write_circle (cursor : in pac_doc_circles.cursor) is
			use pac_doc_circles;
		begin
			section_mark (section_circle, HEADER);
			write_circle (element (cursor));
			write (keyword => keyword_width, parameters => to_string (element (cursor).width));
			section_mark (section_circle, FOOTER);
		end write_circle;

		
		-- CS rename to write_zone
		procedure write_polygon (cursor : in pac_doc_zones.cursor) is 
			use pac_doc_zones;
			zone : type_doc_zone renames element (cursor);
		begin
			section_mark (section_zone, HEADER);
			section_mark (section_contours, HEADER);		
			write_polygon_segments (zone);
			section_mark (section_contours, FOOTER);
			section_mark (section_zone, FOOTER);
		end write_polygon;

		
		procedure write_placeholder (cursor : in pac_text_placeholders.cursor) is 
			ph : type_text_placeholder renames element (cursor);
		begin
			section_mark (section_placeholder, HEADER);
			
			write (keyword => keyword_meaning,
				parameters => to_string (element (cursor).meaning));
			
			----
			-- CS rework:
			
			write (keyword => keyword_position, 
				parameters => to_string (get_position (ph), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (ph.size)); -- size 1.000
			
			write (keyword => keyword_linewidth,
				parameters => to_string (ph.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (ph.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (ph.alignment.vertical));

			-- CS use et_alignment.to_string 
			----
			
			section_mark (section_placeholder, FOOTER);
		end write_placeholder;


		procedure write_text (cursor : in pac_doc_texts.cursor) is 
			t : type_doc_text renames element (cursor);
		begin
			section_mark (section_text, HEADER);
			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));

			----
			-- CS rework:
			
			write (keyword => keyword_position, 
				parameters => to_string (get_position (t), FORMAT_2));
				-- position x 0.000 y 5.555 rotation 0.00
			
			write (keyword => keyword_size, 
				parameters => to_string (t.size)); -- size 1.000
			
			write (keyword => keyword_linewidth,
				parameters => to_string (t.line_width));
				
			write (keyword => keyword_alignment, 
				parameters =>
					keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
					keyword_vertical   & space & to_string (t.alignment.vertical));

			-- CS use et_alignment.to_string 
			----
				
			section_mark (section_text, FOOTER);
		end write_text;

		
	begin
		log (text => "write assembly documentation", level => log_threshold);

		
		section_mark (section_assembly_doc, HEADER);

		-- top
		section_mark (section_top, HEADER);
		iterate (packge.assy_doc.top.lines, write_line'access);
		iterate (packge.assy_doc.top.arcs, write_arc'access);
		iterate (packge.assy_doc.top.circles, write_circle'access);
		iterate (packge.assy_doc.top.zones, write_polygon'access);
		iterate (packge.assy_doc.top.texts, write_text'access);
		iterate (packge.assy_doc.top.placeholders, write_placeholder'access);
		section_mark (section_top, FOOTER);
		
		-- bottom
		section_mark (section_bottom, HEADER);
		iterate (packge.assy_doc.bottom.lines, write_line'access);
		iterate (packge.assy_doc.bottom.arcs, write_arc'access);
		iterate (packge.assy_doc.bottom.circles, write_circle'access);
		iterate (packge.assy_doc.bottom.zones, write_polygon'access);
		iterate (packge.assy_doc.bottom.texts, write_text'access);
		iterate (packge.assy_doc.bottom.placeholders, write_placeholder'access);
		section_mark (section_bottom, FOOTER);

		section_mark (section_assembly_doc, FOOTER);
	end write_assy_doc;

	
end et_package_write_assy_doc;
