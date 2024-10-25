------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SILKSCREEN                                    --
--                                                                          --
--                              B o d y                                     --
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

package body et_silkscreen is

	procedure mirror_lines (
		lines	: in out pac_silk_lines.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_silk_lines.list;

		procedure query_line (c : in pac_silk_lines.cursor) is
			line : type_silk_line := element (c);
		begin
			mirror (line, axis);
			result.append (line);
		end query_line;
		
	begin
		lines.iterate (query_line'access);
		lines := result;
	end mirror_lines;


	
	procedure rotate_lines (
		lines	: in out pac_silk_lines.list;
		angle	: in type_rotation_model)
	is
		result : pac_silk_lines.list;

		procedure query_line (c : in pac_silk_lines.cursor) is
			line : type_silk_line := element (c);
		begin
			rotate_by (line, angle);
			result.append (line);
		end query_line;

	begin
		lines.iterate (query_line'access);
		lines := result;
	end rotate_lines;


	
	procedure move_lines (
		lines	: in out pac_silk_lines.list;
		offset	: in type_distance_relative)
	is
		result : pac_silk_lines.list;

		procedure query_line (c : in pac_silk_lines.cursor) is
			line : type_silk_line := element (c);
		begin
			move_by (line, offset);
			result.append (line);
		end query_line;

	begin
		lines.iterate (query_line'access);
		lines := result;
	end move_lines;




	

	procedure mirror_arcs (
		arcs	: in out pac_silk_arcs.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_silk_arcs.list;

		procedure query_arc (c : in pac_silk_arcs.cursor) is
			arc : type_silk_arc := element (c);
		begin
			mirror (arc, axis);
			result.append (arc);
		end query_arc;
		
	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end mirror_arcs;

	

	procedure rotate_arcs (
		arcs	: in out pac_silk_arcs.list;
		angle	: in type_rotation_model)
	is
		result : pac_silk_arcs.list;

		procedure query_arc (c : in pac_silk_arcs.cursor) is
			arc : type_silk_arc := element (c);
		begin
			rotate_by (arc, angle);
			result.append (arc);
		end query_arc;

	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end rotate_arcs;


	procedure move_arcs (
		arcs	: in out pac_silk_arcs.list;
		offset	: in type_distance_relative)		
	is
		result : pac_silk_arcs.list;

		procedure query_arc (c : in pac_silk_arcs.cursor) is
			arc : type_silk_arc := element (c);
		begin
			move_by (arc, offset);
			result.append (arc);
		end query_arc;

	begin
		arcs.iterate (query_arc'access);
		arcs := result;
	end move_arcs;





	procedure mirror_circles (
		circles	: in out pac_silk_circles.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_silk_circles.list;

		procedure query_circle (c : in pac_silk_circles.cursor) is
			circle : type_silk_circle := element (c);
		begin
			mirror (circle, axis);
			result.append (circle);
		end query_circle;
		
	begin
		circles.iterate (query_circle'access);
		circles := result;
	end mirror_circles;


	procedure rotate_circles (
		circles	: in out pac_silk_circles.list;
		angle	: in type_rotation_model)		
	is
		result : pac_silk_circles.list;

		procedure query_circle (c : in pac_silk_circles.cursor) is
			circle : type_silk_circle := element (c);
		begin
			rotate_by (circle, angle);
			result.append (circle);
		end query_circle;

	begin
		circles.iterate (query_circle'access);		
		circles := result;
	end rotate_circles;			


	procedure move_circles (
		circles	: in out pac_silk_circles.list;
		offset	: in type_distance_relative)		
	is
		result : pac_silk_circles.list;

		procedure query_circle (c : in pac_silk_circles.cursor) is
			circle : type_silk_circle := element (c);
		begin
			move_by (circle, offset);
			result.append (circle);
		end query_circle;

	begin
		circles.iterate (query_circle'access);
		circles := result;
	end move_circles;



	procedure mirror_contours (
		contours	: in out pac_silk_contours.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)		
	is
		result : pac_silk_contours.list;

		procedure query_contour (c : in pac_silk_contours.cursor) is
			contour : type_silk_contour := element (c);
		begin
			mirror (contour, axis);
			result.append (contour);
		end query_contour;
		
	begin
		contours.iterate (query_contour'access);
		contours := result;
	end mirror_contours;


	procedure rotate_contours (
		contours	: in out pac_silk_contours.list;
		angle		: in type_rotation_model)		
	is
		result : pac_silk_contours.list;

		procedure query_contour (c : in pac_silk_contours.cursor) is
			contour : type_silk_contour := element (c);
		begin
			rotate_by (contour, angle);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);		
		contours := result;
	end rotate_contours;			


	procedure move_contours (
		contours	: in out pac_silk_contours.list;
		offset		: in type_distance_relative)		
	is
		result : pac_silk_contours.list;

		procedure query_contour (c : in pac_silk_contours.cursor) is
			contour : type_silk_contour := element (c);
		begin
			move_by (contour, offset);
			result.append (contour);
		end query_contour;

	begin
		contours.iterate (query_contour'access);
		contours := result;
	end move_contours;


	

	procedure mirror_texts (
		texts	: in out pac_silk_texts.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_silk_texts.list;

		procedure query_text (c : in pac_silk_texts.cursor) is
			text : type_silk_text := element (c);
		begin
			mirror_text (text, axis);
			mirror_vector_text (text.vectors, axis);
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end mirror_texts;


	procedure rotate_texts (
		texts	: in out pac_silk_texts.list;
		angle	: in type_rotation_model)
	is
		result : pac_silk_texts.list;

		procedure query_text (c : in pac_silk_texts.cursor) is
			text : type_silk_text := element (c);
		begin
			rotate_text (text, angle);
			rotate_vector_text (text.vectors, angle);
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end rotate_texts;


	procedure move_texts (
		texts	: in out pac_silk_texts.list;
		offset	: in type_distance_relative)
	is
		result : pac_silk_texts.list;

		procedure query_text (c : in pac_silk_texts.cursor) is
			text : type_silk_text := element (c);
		begin
			move_text (text, offset);
			move_vector_text (text.vectors, offset);
			result.append (text);
		end query_text;
		
	begin
		texts.iterate (query_text'access);		
		texts := result;
	end move_texts;
	

	
	procedure line_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_lines.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_silk_lines;
		line : type_silk_line;
	begin
		line := element (cursor);
		log (text => "silk screen line face" & to_string (face) & space 
			 & to_string (type_line (line))
			 & " width" & to_string (line.width), level => log_threshold);
	end line_silk_screen_properties;

	
	procedure arc_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_arcs.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_silk_arcs;
		arc : type_silk_arc;
	begin
		arc := element (cursor);
		log (text => "silk screen arc face" & to_string (face) & space 
			 & to_string (type_arc (arc))
			 & " width" & to_string (arc.width), level => log_threshold);
	end arc_silk_screen_properties;

	
	procedure circle_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_circles.cursor;
		log_threshold 	: in type_log_level)
	is
		use pac_silk_circles;
	begin
		log (text => "silk screen circle face" & to_string (face)
			 & to_string (element (cursor)),
			level => log_threshold);
	end;


	procedure text_silk_screen_properties (
		face			: in type_face;
		cursor			: in pac_silk_texts.cursor;
		log_threshold 	: in type_log_level) 
	is
		use et_text.pac_text_content;
		text : type_silk_text renames element (cursor);
	begin
		log (text => "silk screen text face" & to_string (face) & space
			 & "content '" & to_string (text.content) & "'", level => log_threshold);

		log_indentation_up;
		-- CS log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_silk_screen_properties;
	
end et_silkscreen;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
