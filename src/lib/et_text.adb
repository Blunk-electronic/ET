------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.characters.handling;	use ada.characters.handling;

with et_general;
with et_exceptions;				use et_exceptions;


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
		line : in type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in count_type)
		return type_text_alignment 
	is
		function f (line : in type_fields_of_line; position : in count_type) 
			return string renames get_field;
		
		alignment : type_text_alignment; -- to be returned

		place : count_type := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= field_count (line) loop

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


	

	function to_string (text_content : in pac_text_content.bounded_string) return string is begin
		return pac_text_content.to_string (text_content);
	end to_string;

	function to_content (content : in string) return pac_text_content.bounded_string is begin
		return pac_text_content.to_bounded_string (content);
	end to_content;

	function is_empty (content : in pac_text_content.bounded_string) return boolean is begin
		if pac_text_content.length (content) > 0 then -- contains something -> not empty
			return false;
		else
			return true; -- contains nothing -> is empty
		end if;
	end is_empty;

	function characters_valid (
		content		: in pac_text_content.bounded_string;
		characters	: in character_set := valid_characters) 
		return boolean 
	is
		use pac_text_content;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> content,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "Text " & enclose_in_quotes (to_string (content))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				 & " !"
				);
			return false;
		else
			return true;
		end if;
	end characters_valid;

	procedure replace_invalid_characters (
		content		: in out pac_text_content.bounded_string;
		replace_by	: in character := replace_by_default;
		characters	: in character_set := valid_characters)
	is
		use pac_text_content;
		invalid_character_position : natural := 0;

		l_max : natural;
	begin
		if characters_valid (content) then
			null;
		else
			log (WARNING, "Replacing invalid characters in text " 
				& enclose_in_quotes (to_string (content))
				& " by " & enclose_in_quotes (replace_by) & " !");

			-- To prevent an infintive loop, we test for invalid characters
			-- no more often than the length of the given content:
			l_max := length (content);
			
			for p in 0 .. l_max loop
				
				invalid_character_position := index (
					source	=> content,
					set 	=> characters,
					test 	=> outside);

				-- If there is an invalid character, replace it at the detected
				-- position. Eventually there are no more invalid characters
				-- and the loop ends prematurely.
				if invalid_character_position > 0 then
					replace_element (content, invalid_character_position, replace_by);
				else
					exit;
				end if;
				
			end loop;
		end if;
	end replace_invalid_characters;


	
	procedure check_text_content_length (content : in string) is
	-- Tests if the content is longer than allowed.
	begin
		if content'length > text_length_max then
			log (ERROR, "max. number of characters for a text field is" 
				 & positive'image (text_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_text_content_length;


	procedure toggle_rotation (rotation : in out type_rotation_documentation) is
	begin
		case rotation is
			when HORIZONTAL	=> rotation := VERTICAL;
			when VERTICAL	=> rotation := HORIZONTAL;
		end case;
	end toggle_rotation;


-- GENERIC PART
	
	package body generic_pac_text is

		-- With this line uncommented the linker does not output any errors:
		function to_text_size (size : in pac_geometry.type_distance) return type_text_size is

		-- With this line uncommented the linker outputs errors like "undefined reference ..."
		-- function to_text_size (size : in type_distance) return type_text_size is
			
		-- Converts given distance to type_text_size. Raises error on excessive text size.
			function to_string (
				size		: in type_text_size;
				preamble	: in boolean := true) return string is
			-- Returns the given text size as string.
			begin
				if preamble then
					return "size " & pac_geometry.to_string (size);
				else
					return pac_geometry.to_string (size);
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
		
		
		procedure validate_text_size (size : in pac_geometry.type_distance) is
		begin
			if size not in type_text_size then
				log (ERROR, "text size invalid ! Allowed range is" 
					& to_string (type_text_size'first) & " .."
					& to_string (type_text_size'last),
					console => true);
				raise constraint_error;
			end if;
		end validate_text_size;

		procedure validate_text_line_width (width : in pac_geometry.type_distance) is
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
			return pac_geometry.type_rotation is
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
			rotation_add	: in pac_geometry.type_rotation)
			return pac_geometry.type_rotation is
		begin
			return to_rotation (rotation_doc) + rotation_add;
		end;
		
		procedure warning_rotation_outside_range is
		begin
			log (WARNING, "rotation of documentational text invalid. Must be 0 or 90 degrees !");
		end;

		function snap (rotation : in pac_geometry.type_rotation) return type_rotation_documentation is
			offset : constant pac_geometry.type_rotation := 45.0 - pac_geometry.type_rotation'small;
			r1 : pac_geometry.type_rotation;
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
			r : constant pac_geometry.type_rotation := to_rotation (rotation);
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

		function to_lines (char : in type_character) return pac_vector_text_lines.list is
			use pac_vector_text_lines;
			result : pac_vector_text_lines.list;
		begin
			for l in char'first .. char'last loop
				
				append (result, (
					start_point => type_point (set (char (l).start_x, char (l).start_y)),
					end_point   => type_point (set (char (l).end_x, char (l).end_y))
					));

			end loop;

			return result;
		end to_lines;

		-- This function sorts lines by the distance of their start points
		-- to the origin.
		-- CS: The sorting could be improved but seems sufficient for now.
		function "<" (left, right : in type_line) return boolean is 
			--result : boolean := false;
		begin
			if left.start_point < right.start_point then
				return true;
			else
				return false;
			end if;
		end "<";

			
		
		function vectorize_text (
			content		: in pac_text_content.bounded_string; -- MUST CONTAIN SOMETHING !
			size		: in type_text_size;
			rotation	: in pac_geometry.type_rotation; 
			position	: in type_point; -- the anchor point of the text (where the origin is)
			mirror		: in type_vector_text_mirrored := vector_text_mirror_default;
			line_width	: in pac_geometry.type_distance_positive;
			alignment	: in type_text_alignment := vector_text_alignment_default)
			return pac_vector_text_lines.list
		is
			use et_general;
			use pac_vector_text_lines;

			-- We return a list of lines. In the course of this function
			-- this list gets filled with the lines of vectorized characters:
			result : pac_vector_text_lines.list;

			-- This is the text we will be displaying. It will be read
			-- character by character. Each character will be mapped 
			-- to a vectorized character (which is a list of lines):
			text : constant string := pac_text_content.to_string (content);

			package sorting is new generic_sorting;
			use sorting;

			-- Since there is a line width, the text position must be changed slightly:
			offset_due_to_line_width : constant type_distance_relative :=
				to_distance_relative (set (x => line_width * 0.5, y => line_width * 0.5));
			
			-- This indicates the position of the character being processed:
			place : positive := 1;

			-- The space between the lower left corners of two adjacent characters:
			-- It must be adjusted according to the given text size:
			spacing : constant type_distance_positive := size * (0.25 + type_character_width'last);
			
			-- The scaling is done so that text height and width are
			-- independed of the line width.
			-- CS: Currently the scaling factor M applies to X and Y axis
			-- in the same way. Scaling in X might be slightly different:
			M : constant type_text_size := size - line_width;

			-- For alignment we need the total length of the text:
			text_length : constant type_distance_positive := line_width * 0.5 +
					(text'length - 1) * (spacing * M);

			text_length_half : constant type_distance_positive := text_length * 0.5;

			text_height : constant type_distance_positive := size;
			text_height_half : constant type_distance_positive := size * 0.5;
			
			procedure scale_line (l : in out type_line) is 
				Sx : constant type_distance := get_x (l.start_point);
				Sy : constant type_distance := get_y (l.start_point);
				Ex : constant type_distance := get_x (l.end_point);
				Ey : constant type_distance := get_y (l.end_point);
			begin
				l.start_point := type_point (set (Sx * M, Sy * M));
				l.end_point   := type_point (set (Ex * M, Ey * M));
			end scale_line;
			
			procedure move_character (lines : in out pac_vector_text_lines.list) is
				
				-- Here we collect the lines of the moved character.
				-- scratch will overwrite the given lines at the end of this procedure:
				scratch : pac_vector_text_lines.list;

				procedure query_line (c : in pac_vector_text_lines.cursor) is
					l : type_line := element (c);
				begin
					-- According to the given text size, the line is now 
					-- to be scaled:
					scale_line (l);

					-- Move the line by offset_due_to_line_width (see above):
					pac_shapes.move_by (
						line	=> pac_shapes.type_line (l),
						offset	=> offset_due_to_line_width);
										   
					-- Move the line to the right according to the
					-- position of the character inside the text. 
					-- CS: depends on alignment ?
					pac_shapes.move_by (
						line	=> pac_shapes.type_line (l),
						offset	=> to_distance_relative (set (
									x => (place - 1) * spacing,
									y => zero)));

					-- Collect the line in scratch:
					append (scratch, l);
				end query_line;
				
			begin
				iterate (lines, query_line'access); -- query the lines of the character
				lines := scratch; -- replace old lines by new lines
			end move_character;
			
			-- This procedure merges the given vectorized character
			-- with the result. The result is a collection of lines.
			procedure add (char : in type_character) is 
				lines : pac_vector_text_lines.list := to_lines (char);
			begin
				move_character (lines);
				merge (target => result, source => lines);
			end add;

			procedure finalize is
				scratch : pac_vector_text_lines.list;

				procedure query_line (c : in pac_vector_text_lines.cursor) is 
					l : type_line := element (c);

					procedure align_vertical is begin
						case alignment.vertical is
							when BOTTOM => 
								null; -- text is already computed for bottom alignment. nothing to do
							
							when CENTER =>
								pac_shapes.move_by (
									line	=> pac_shapes.type_line (l),
									offset	=> to_distance_relative (set (zero, - text_height_half)));

							when TOP =>
								pac_shapes.move_by (
									line	=> pac_shapes.type_line (l),
									offset	=> to_distance_relative (set (zero, - text_height)));

						end case;
					end align_vertical;
			
				begin -- query_line
					
					-- Align the text with the origin:
					case alignment.horizontal is
						when LEFT => 
							-- text is already computed for left alignment. so no need to align horizontal.
							align_vertical;

						when CENTER =>
							pac_shapes.move_by (
								line	=> pac_shapes.type_line (l),
								offset	=> to_distance_relative (set (- text_length_half, zero)));

							align_vertical;
							
						when RIGHT =>
							pac_shapes.move_by (
								line	=> pac_shapes.type_line (l),
								offset	=> to_distance_relative (set (- text_length, zero)));
							
							align_vertical;
					end case;
					
					-- Rotate the text (about the origin) if required:
					if rotation /= zero_rotation then
						pac_shapes.rotate_by (
							line		=> pac_shapes.type_line (l),
							rotation	=> rotation);
					end if;

					-- Mirror the text if required:
					if mirror = YES then
						pac_shapes.mirror (
							line	=> pac_shapes.type_line (l),
							axis	=> Y);
					end if;
					
					-- Move the text by the given position. 
					-- The given position is the anchor point of the text.
					pac_shapes.move_by (
						line	=> pac_shapes.type_line (l),
						offset	=> to_distance_relative (position));

					append (scratch, l);
				end query_line;
			
			begin -- finalize
				--put_line ("length " & to_string (text_length));
				
				iterate (result, query_line'access);
				result := scratch;
			end finalize;
			
		begin -- vectorize_text
			-- Read the text to be displayed character by character and
			-- map from character to the corresponding vectorized character:
			for c in text'first .. text'last loop
				place := c;
				
				case text (c) is
					when 'A' => add (capital_a);
					when 'B' => add (capital_b);
					when 'C' => add (capital_c);
					when 'D' => add (capital_d);
					when 'E' => add (capital_e);
					when 'F' => add (capital_f);
					when 'G' => add (capital_g);
					when 'H' => add (capital_h);
					when 'I' => add (capital_i);
					when 'J' => add (capital_j);
					when 'K' => add (capital_k);
					when 'L' => add (capital_l);
					when 'M' => add (capital_m);
					when 'N' => add (capital_n);
					when 'O' => add (capital_o);
					when 'P' => add (capital_p);
					when 'Q' => add (capital_q);
					when 'R' => add (capital_r);
					when 'S' => add (capital_s);
					when 'T' => add (capital_t);
					when 'U' => add (capital_u);
					when 'V' => add (capital_v);
					when 'W' => add (capital_w);
					when 'X' => add (capital_x);
					when 'Y' => add (capital_y);
					when 'Z' => add (capital_z);

					when 'a' => add (small_a);
					when 'b' => add (small_b);
					when 'c' => add (small_c);
					when 'd' => add (small_d);
					when 'e' => add (small_e);
					when 'f' => add (small_f);
					when 'g' => add (small_g);
					when 'h' => add (small_h);
					when 'i' => add (small_i);
					when 'j' => add (small_j);
					when 'k' => add (small_k);
					when 'l' => add (small_l);
					when 'm' => add (small_m);
					when 'n' => add (small_n);
					when 'o' => add (small_o);
					when 'p' => add (small_p);
					when 'q' => add (small_q);
					when 'r' => add (small_r);
					when 's' => add (small_s);
					when 't' => add (small_t);
					when 'u' => add (small_u);
					when 'v' => add (small_v);
					when 'w' => add (small_w);
					when 'x' => add (small_x);
					when 'y' => add (small_y);
					when 'z' => add (small_z);
					
					when '0' => add (digit_0);
					when '1' => add (digit_1);
					when '2' => add (digit_2);
					when '3' => add (digit_3);
					when '4' => add (digit_4);
					when '5' => add (digit_5);
					when '6' => add (digit_6);
					when '7' => add (digit_7);
					when '8' => add (digit_8);
					when '9' => add (digit_9);

					when '+' => add (special_plus);
					when '-' => add (special_dash);
					when '_' => add (special_underline);
					when '/' => add (special_forward_slash);
					when ' ' => null;
					
					when others => 
						raise syntax_error_1 with
						"ERROR: Invalid character in " & enclose_in_quotes (text) 
							 & " at position" & positive'image (place) & " !";

				end case;
			end loop;

			-- Align, mirror and move the text to the final position:
			finalize;
			
			return result;
		end vectorize_text;

		
	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
