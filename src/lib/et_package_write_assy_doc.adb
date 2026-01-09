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



with ada.text_io;				use ada.text_io;
-- with ada.characters.handling;	use ada.characters.handling;
-- with ada.strings; 				use ada.strings;

with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_package_sections;				use et_package_sections;

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_text_content;					use et_text_content;
with et_assy_doc;						use et_assy_doc;
with et_assy_doc.packages;			use et_assy_doc.packages;

with et_general_rw;						use et_general_rw;
with et_board_write;					use et_board_write;


package body et_package_write_assy_doc is

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

		procedure write_placeholder (cursor : in pac_text_placeholders.cursor) is begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;


		procedure write_text (cursor : in pac_doc_texts.cursor) is begin
			text_begin;
			write (keyword => keyword_content, wrap => true,
				parameters => to_string (element (cursor).content));

			write_text_properties (element (cursor));
			text_end;
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
