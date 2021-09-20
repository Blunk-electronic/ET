------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        ASSEMBLY DOCUMENTAION                             --
--                                                                          --
--                              B o d y                                     --
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
--   to do:


with ada.strings;	 			use ada.strings;

package body et_assy_doc is

	procedure line_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_lines.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_doc_lines;
		line : type_doc_line;
	begin
		line := element (cursor);
		log (text => "assembly doc line face" & to_string (face) & space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_assy_doc_properties;

	
	procedure arc_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_arcs.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_doc_arcs;
		arc : type_doc_arc;
	begin
		arc := element (cursor);
		log (text => "assembly doc arc face" & to_string (face) & space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_assy_doc_properties;

	
	procedure circle_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_doc_circles.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_doc_circles;
	begin
		log (text => "assembly doc circle face" & to_string (face) & space 
			 & to_string (element (cursor)),
			level => log_threshold);
	end;


	procedure text_assy_doc_properties (
		face			: in type_face;
		cursor			: in pac_texts_fab_with_content.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_texts_fab_with_content;
		use et_text.pac_text_content;
		text : type_text_fab_with_content;
	begin
		text := element (cursor);
		log (text => "assembly doc text face" & to_string (face) & space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		-- CS log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_assy_doc_properties;

	
end et_assy_doc;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
