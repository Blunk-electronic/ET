------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             STOP MASK                                    --
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

with ada.strings;				use ada.strings;

package body et_stop_mask is


	procedure arc_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_arcs.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_arcs;
		arc : type_stop_arc;
	begin
		arc := element (cursor);
		log (text => "stop mask arc face" & to_string (face) & space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width),
			 level => log_threshold);
	end arc_stop_mask_properties;

	procedure circle_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_circles.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_circles;
	begin
		log (text => "stop mask circle face" & to_string (face) & space 
			& to_string (element (cursor)),
			level => log_threshold);
	end;

	
	procedure line_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_stop_lines.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_stop_lines;
		line : type_stop_line;
	begin
		line := element (cursor);
		log (text => "stop mask line face" & to_string (face) & space
			 & to_string (type_line (line))
			 & " width" & to_string (line.width),
			 level => log_threshold);
	end line_stop_mask_properties;

	
	procedure text_stop_mask_properties (
		face			: in type_face;
		cursor			: in pac_texts_fab_with_content.cursor;
		log_threshold 	: in et_string_processing.type_log_level) 
	is
		use pac_texts_fab_with_content;
		use et_text.pac_text_content;
		text : type_text_fab_with_content;
	begin
		text := element (cursor);
		log (text => "stop mask text face" & to_string (face) & space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		-- CS log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_stop_mask_properties;

	
end et_stop_mask;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
