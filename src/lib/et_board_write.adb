------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOARD WRITE                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with et_axes;						use et_axes;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_text;						use et_text;
with et_text_content;				use et_text_content;
with et_rotation_docu;				use et_rotation_docu;
with et_alignment;					use et_alignment;
with et_exceptions;					use et_exceptions;
with et_keywords;					use et_keywords;
with et_directions;					use et_directions;


package body et_board_write is

	--procedure write_text_properties (t : in et_packages.type_text'class) is
		--use et_packages;
	--begin
---- 		write (keyword => keyword_position, parameters => position (text.position) & 
---- 			space & keyword_rotation & to_string (get_angle (text.position))
---- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		--write (keyword => keyword_position, parameters => position (t.position));
			---- position x 0.000 y 5.555 rotation 0.00
		
		--write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		--write (keyword => keyword_line_width, parameters => to_string (t.line_width));
		--write (keyword => keyword_alignment, parameters =>
			--keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			--keyword_vertical   & space & to_string (t.alignment.vertical));
		
		---- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	--end write_text_properties;

	
	procedure write_text_properties (
		t : in type_text_fab'class) 
	is begin
		-- CS rework !
		
-- 		write (keyword => keyword_position, parameters => position (text.position) & 
-- 			space & keyword_rotation & to_string (get_angle (text.position))
-- 			  ); -- position x 0.000 y 5.555 rotation 0.00

		write (keyword => keyword_position, parameters => 
			to_string (get_position (t), FORMAT_2));
			-- position x 0.000 y 5.555 rotation 0.00
		
		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_linewidth, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
			keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
			keyword_vertical   & space & to_string (t.alignment.vertical));

		-- CS use et_alignment.to_string 
	end write_text_properties;

	
	
	procedure write_text_properties_with_face (
		t		: in type_text_fab'class;
		face	: in type_face) 
	is 
		position : type_package_position;
	begin
		-- CS rework !

		-- Assemble the text position:
		set_position (position, get_position (t));
		set_face (position, face);
					  
		write (keyword => keyword_position, parameters => 
			   to_string (position, FORMAT_2)); 
		-- position x 0.000 y 5.555 rotation 0.00 face top

		write (keyword => keyword_size, parameters => to_string (t.size)); -- size 1.000
		
		write (keyword => keyword_linewidth, parameters => to_string (t.line_width));
		write (keyword => keyword_alignment, parameters =>
				keyword_horizontal & space & to_string (t.alignment.horizontal) & space &
				keyword_vertical   & space & to_string (t.alignment.vertical)
			  );

		-- CS use et_alignment.to_string 
		
		-- CS write (keyword => keyword_hidden, parameters => space & to_lower (boolean'image (text.hidden)));
	end write_text_properties_with_face;

	
	
	procedure write_text (cursor : in pac_texts_fab_with_content.cursor) is
		use pac_texts_fab_with_content;
	begin
		text_begin;
		write (keyword => keyword_content, wrap => true,
			parameters => to_string (element (cursor).content));
		write_text_properties (element (cursor));
		text_end;
	end write_text;

	--procedure write_text (cursor : in pac_conductor_texts_package.cursor) is
		--use pac_conductor_texts_package;
	--begin
		--text_begin;
		--write (keyword => keyword_content, wrap => true,
			--parameters => to_string (element (cursor).content));
		---- CS write_text_properties (element (cursor));
		--text_end;
	--end write_text;

	
	procedure write_width (width : in type_track_width) is begin
		write (keyword => keyword_width, parameters => to_string (width));
	end;

	procedure write_fill_linewidth (width : in type_track_width) is begin
		write (keyword => keyword_linewidth, parameters => to_string (width));
	end;

	
	procedure write_line (line : in type_line'class) is begin
		write (keyword => keyword_start, parameters => to_string (get_A (line), FORMAT_2));
		write (keyword => keyword_end  , parameters => to_string (get_B (line), FORMAT_2));
	end write_line;

	
	procedure write_arc (arc : in type_arc'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (arc), FORMAT_2));
		write (keyword => keyword_start, parameters => to_string (get_A (arc), FORMAT_2));
		write (keyword => keyword_end, parameters => to_string (get_B (arc), FORMAT_2));
		write (keyword => keyword_direction, parameters => to_string (get_direction (arc)));
	end write_arc;

	
	procedure write_circle (circle : in type_circle'class) is begin
		write (keyword => keyword_center, parameters => to_string (get_center (circle), FORMAT_2));
		write (keyword => keyword_radius, parameters => to_string (get_radius (circle)));
	end write_circle;

		
	
	procedure write_fill_style (fill_style : in type_fill_style) is
	begin
		write (keyword => keyword_fill_style, parameters => to_string (fill_style));
	end;


	
	
	procedure write_polygon_segments (
		polygon : in type_contour'class)
	is
		use pac_segments;
		
		procedure query_segment (c : in pac_segments.cursor) is begin
			case element (c).shape is
				
				when LINE =>
					line_begin;
					write_line (element (c).segment_line);
					line_end;

				when ARC =>
					arc_begin;
					write_arc (element (c).segment_arc);
					arc_end;

			end case;
		end query_segment;		

		contours : type_segments := get_segments (polygon);
		
	begin				
		if contours.circular then

			circle_begin;
			write_circle (contours.circle);
			circle_end;

		else
			contours.segments.iterate (query_segment'access);
		end if;
		
	end write_polygon_segments;
	


	
	

	
	
	procedure fill_zone_begin is begin section_mark (section_zone, HEADER); end;
	procedure fill_zone_end   is begin section_mark (section_zone, FOOTER); end;
	procedure cutout_zone_begin is begin section_mark (section_cutout_zone, HEADER); end;
	procedure cutout_zone_end   is begin section_mark (section_cutout_zone, FOOTER); end;
	procedure contours_begin is begin section_mark (section_contours, HEADER); end;
	procedure contours_end   is begin section_mark (section_contours, FOOTER); end;

	
	
end et_board_write;
