------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with et_string_processing;

package body et_text is


-- TEXT ALIGNMENT
	function to_string (alignment : in type_text_alignment_horizontal) return string is begin
		return to_lower (type_text_alignment_horizontal'image (alignment));
	end;

	function to_alignment_horizontal (alignment : in string) return type_text_alignment_horizontal is begin
		return type_text_alignment_horizontal'value (alignment);
	end;
	
	function to_string (alignment : in type_text_alignment_vertical) return string is begin
		return to_lower (type_text_alignment_vertical'image (alignment));
	end;

	function to_alignment_vertical (alignment : in string) return type_text_alignment_vertical is begin
		return type_text_alignment_vertical'value (alignment);
	end;

	function to_alignment (
		line : in et_string_processing.type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in positive)
		return type_text_alignment is

		use et_string_processing;

		function f (line : in type_fields_of_line; position : in positive) return string 
			renames et_string_processing.field;
		
		alignment : type_text_alignment; -- to be returned

		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the "horizontal" the horizontal alignment
			if f (line, place) = keyword_horizontal then
				alignment.horizontal := to_alignment_horizontal (f (line, place + 1));

			-- We expect after the "vertical" the vertical alignment
			elsif f (line, place) = keyword_vertical then
				alignment.vertical := to_alignment_vertical (f (line, place + 1));
				
			else
				log (ERROR, "alignment invalid: " & (f (line, place)));
				raise constraint_error;
			end if;
				
			place := place + 2;
		end loop;
		
		return alignment;
	end to_alignment;

	
	function to_string (alignment : in type_text_alignment) return string is
	begin
		return "alignment (hor./vert.) "
			& to_string (alignment.horizontal)
			& " / "
			& to_string (alignment.vertical);
	end to_string;


	function to_string (family : in pac_font_family.bounded_string) return string is begin
		return pac_font_family.to_string (family);
	end to_string;

	function to_family (family : in string) return pac_font_family.bounded_string is begin
		return pac_font_family.to_bounded_string (family);
	end to_family;


	

	function to_string (text_content : in type_text_content.bounded_string) return string is begin
		return type_text_content.to_string (text_content);
	end to_string;

	function to_content (content : in string) return type_text_content.bounded_string is begin
		return type_text_content.to_bounded_string (content);
	end to_content;

	
	procedure check_text_content_length (content : in string) is
	-- Tests if the content is longer than allowed.
		use et_string_processing;
	begin
		if content'length > text_length_max then
			log (ERROR, "max. number of characters for a text field is" 
				 & positive'image (text_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_text_content_length;


	
	
	package body text is

		use et_string_processing;
	
		function to_text_size (size : in type_distance) return type_text_size is
		-- Converts given distance to type_text_size. Raises error on excessive text size.
			use et_string_processing;
			
			function to_string (
				size		: in type_text_size;
				preamble	: in boolean := true) return string is
			-- Returns the given text size as string.
			begin
				if preamble then
					return "size " & pac_shapes.geometry.to_string (size);
				else
					return pac_shapes.geometry.to_string (size);
				end if;
			end to_string;

		begin
			if size not in type_text_size then
				log (ERROR, "text " 
					& to_string (size => size, preamble => true)  
					& " out of range !",
					console => true);

				log (text => "Allowed range is " & to_string (type_text_size'first, preamble => false) & " .. "
					& to_string (type_text_size'last, preamble => false),
					console => true);

				raise constraint_error;
			end if;
			return size;
		end to_text_size;
		
		
		procedure validate_text_size (size : in type_distance) is
		-- Checks whether given text size is in range of type_text_size.
		begin
			if size not in type_text_size then
				log (ERROR, "text size invalid ! Allowed range is" 
					& to_string (type_text_size'first) & " .."
					& to_string (type_text_size'last),
					console => true);
				raise constraint_error;
			end if;
		end validate_text_size;

		procedure validate_text_line_width (width : in type_distance) is
		-- Checks whether given line width is in range of type_text_line_width
		begin
			if width not in type_text_line_width then
				log (ERROR, "line width invalid ! Allowed range is" 
					& to_string (type_text_line_width'first) & " .."
					& to_string (type_text_line_width'last),
					console => true);
				raise constraint_error;
			end if;
		end validate_text_line_width;





		
		function text_properties (text : in type_text) return string is
		-- Returns the properties of the given text in a long single string.
		begin
			return
				"size" 
				& to_string (text.size)
--				& " line width" & to_string (text.line_width)
-- 				& " rotation" & to_string (rot (text.position))
				& to_string (text.alignment)
				;
		end text_properties;

		function to_rotation (rotation : in type_rotation_documentation) 
			return type_rotation is
		begin
			case rotation is
				when HORIZONTAL => return zero_rotation;
				when VERTICAL => return 90.0;
			end case;
		end to_rotation;
		
		function to_string (rotation : in type_rotation_documentation) 
			return string is
		begin
			if rotation = HORIZONTAL then
				return to_string (zero_rotation);
			else
				return to_string (rotation => 90.0);
			end if;
		end;

		function "+" (
			rotation_doc	: in type_rotation_documentation;
			rotation_add	: in type_rotation)
			return type_rotation is
		begin
			return to_rotation (rotation_doc) + rotation_add;
		end;
		
		procedure warning_rotation_outside_range is
			use et_string_processing;
		begin
			log (WARNING, "rotation of documentational text invalid. Must be 0 or 90 degrees !");
		end;

		function snap (rotation : in type_rotation) return type_rotation_documentation is
			offset : constant type_rotation := 45.0 - type_rotation'small;
			r1 : type_rotation;
			r2 : float;
			r3 : integer;
		begin
			r1 := (abs (rotation) + offset) / 90.0;
			r2 := float'floor (float (r1));
			r3 := integer (r2);

			if r3 rem 2 = 0 then return HORIZONTAL;
			else return VERTICAL;
			end if;
		end;
		
		function to_rotation_doc (rotation : in string) return type_rotation_documentation is
			r : constant type_rotation := to_rotation (rotation);
		begin
			if r = zero_rotation then
				return HORIZONTAL;
				
			elsif r = 90.0 then
				return VERTICAL;
				
			else
				warning_rotation_outside_range;
				return snap (r);
			end if;
		end;



		
	-- VECTORIZED TEXT
		

		function vectorize (
			content		: in type_text_content.bounded_string;
			size		: in type_text_size;
			rotation	: in type_rotation;
			position	: in type_point;
			mirror		: in type_vector_text_mirrored := vector_text_mirror_default;
			ratio		: in type_line_with_to_size_ratio := line_width_to_size_ratio_default;
			alignment	: in type_text_alignment := vector_text_alignment_default)
			return pac_vector_text_lines.list is

			use pac_vector_text_lines;
			result : pac_vector_text_lines.list; -- to be returned

			l : type_vector_text_line;
		begin
			l.end_point := type_point (origin);
			l.start_point := type_point (set (1.0, 1.0));

			append (result, l);

			l.end_point := type_point (set (2.0, 2.0));
			l.start_point := type_point (set (3.0, 3.0));

			append (result, l);

			
			return result;
		end vectorize;

		
	end text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
