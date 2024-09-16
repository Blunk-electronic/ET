------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               B o d y                                    --
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


		
		function text_properties (
			text : in type_text) 
			return string 
		is begin
			return
				"size" & to_string (text.size)
				& to_string (text.alignment);
		end text_properties;


		
		function get_position (text : in type_text_fab)
			return pac_geometry.type_position
		is begin
			return text.position;
		end get_position;
		
		function get_place (text : in type_text_fab)
			return type_vector_model
		is begin
			return text.position.place;
		end get_place;

		
		function get_rotation (text : in type_text_fab)
			return type_rotation
		is begin
			return text.position.rotation;
		end get_rotation;

		
		procedure mirror_text (
			text	: in out type_text_fab;
			axis	: in type_axis_2d := Y)
		is begin
			mirror (text.position.place, axis);
		end mirror_text;

		
		procedure rotate_text (
			text	: in out type_text_fab;
			angle	: in type_rotation)
		is begin
			rotate_by (text.position.place, angle);
			text.position.rotation := add (text.position.rotation, angle);
		end rotate_text;


		procedure move_text (
			text	: in out type_text_fab;
			offset	: in type_distance_relative)
		is begin
			move_by (text.position.place, offset);
		end move_text;

		procedure move_text (
			text	: in out type_text_fab;
			point	: in type_vector_model)
		is begin
			text.position.place := point;
		end move_text;

		
		function text_properties (
			text : in type_text_fab)
			return string 
		is begin
			return text_properties (type_text (text))
				--& " pos " & to_string (type_vector_model (text.position))
				& " pos " & to_string (text.position.place)
				& " line width" & to_string (text.line_width)
				& " rotation" & to_string (get_rotation (text.position));
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


		
		function to_lines (
			char : in type_character) 
			return pac_character_lines.list 
		is
			use pac_character_lines;
			result : pac_character_lines.list;
			scratch : type_character_line;
		begin
			--for l in char.segments'first .. char.segments'last loop
			for l in char.segments'first .. char.segments'last loop

				scratch.start_point := set (char.segments (l).start_x, char.segments (l).start_y);
				scratch.end_point   := set (char.segments (l).end_x,   char.segments (l).end_y);
					
				--append (result, (
					----start_point => type_vector_model (set (char (l).start_x, char (l).start_y)),
					----end_point   => type_vector_model (set (char (l).end_x, char (l).end_y))
					--start_point => to_vector (set (char (l).start_x, char (l).start_y)),
					--end_point   => to_vector (set (char (l).end_x, char (l).end_y))
					--));

				append (result, scratch);
			end loop;

			return result;
		end to_lines;

		
		-- This function sorts lines by the distance of their start points
		-- to the origin.
		-- CS: The sorting could be improved but seems sufficient for now.
		function "<" (
			left, right : in type_character_line) 
			return boolean 
		is begin
			if left.start_point < right.start_point then
				return true;
			else
				return false;
			end if;
		end "<";

			
		
		function vectorize_text (
			content		: in pac_text_content.bounded_string; -- MUST CONTAIN SOMETHING !
			size		: in type_text_size;
			rotation	: in type_rotation; 
			position	: in pac_geometry.type_vector_model; -- the anchor point of the text (where the origin is)
			mirror		: in type_vector_text_mirrored := vector_text_mirror_default;
			line_width	: in pac_geometry.type_distance_positive;
			alignment	: in type_text_alignment := vector_text_alignment_default;
			make_border	: in boolean := false)
			return type_vector_text
		is
			use pac_character_lines;

			-- We return a list of lines. In the course of this function
			-- this list gets filled with the lines of vectorized characters:
			result : type_vector_text := (width => line_width, others => <>);

			-- This is the text content we will be displaying. It will be read
			-- character by character. Each character will be mapped 
			-- to a vectorized character (which is a list of lines):
			text : constant string := pac_text_content.to_string (content);

			package sorting is new pac_character_lines.generic_sorting;


			half_line_width : constant type_float_positive := 
				type_float (line_width) * 0.5;
			
			-- Since there is a line width, the text position must be changed slightly:
			offset_due_to_line_width : constant type_offset :=
				to_offset (half_line_width, half_line_width);
			
			-- This indicates the position of the character being processed:
			place : positive := 1;

			-- The space between the lower left corners of two adjacent characters:
			-- It must be adjusted according to the given text size:
			spacing : constant type_distance_positive := 
				size * (0.25 + type_distance_positive (type_character_width'last));

			
			-- The scaling is done so that text height and width are
			-- independed of the line width.
			-- The scaling factor M applies to X and Y axis in the same way.
			scale_factor : constant type_text_size := size - line_width;

			scale_factor_float : constant type_float_positive := type_float_positive (scale_factor);

			
			-- For alignment we need the total length of the text:
			text_length : constant type_distance_positive := to_distance (half_line_width) +
				type_distance (text'length - 1) * type_distance (spacing * scale_factor);
			-- CS constraint_error raised if text length is zero !
			
			text_length_half : constant type_distance_positive := text_length * 0.5;

			text_height : constant type_distance_positive := size;
			text_height_half : constant type_distance_positive := size * 0.5;

			
			procedure scale_and_move_lines (lines : in out pac_character_lines.list) is
				
				-- Here we collect the lines of the moved character.
				-- scratch will overwrite the given lines at the end of this procedure:
				scratch : pac_character_lines.list;

				procedure query_line (c : in pac_character_lines.cursor) is
					l : type_character_line := element (c);
				begin
					-- According to the given text size, the line is now 
					-- to be scaled:
					scale (l, scale_factor_float);

					-- Move the line by offset_due_to_line_width (see above):
					move_by (
						line	=> l,
						offset	=> offset_due_to_line_width);
										   
					-- Move the line to the right according to the
					-- position of the character inside the text. 
					-- CS: depends on alignment ?
					move_by (
						line	=> l,
						offset	=> to_offset (
									x => type_distance (place - 1) * spacing,
									y => zero));

					-- Collect the line in scratch:
					append (scratch, l);
				end query_line;
				
			begin
				iterate (lines, query_line'access); -- query the lines of the character
				lines := scratch; -- replace old lines by new lines
			end scale_and_move_lines;


			procedure scale_and_move_border (border : in out pac_vectors.list) is 

				procedure align_vertical is begin
					case alignment.vertical is
						when BOTTOM => 
							null; -- already computed for bottom alignment. nothing to do
						
						when CENTER =>
							move_by (border, to_offset (zero, - text_height_half));
							
						when TOP =>
							move_by (border, to_offset (zero, - text_height));
					end case;
				end align_vertical;

			begin -- scale_and_move_border
				scale (border, scale_factor_float);
				move_by (border, offset_due_to_line_width);

				move_by (border, to_offset (
									x => type_distance (place - 1) * spacing,
									y => zero));


				-- CS: Not tested !
				-- Align with the origin:
				case alignment.horizontal is
					when LEFT => 
						-- already computed for left alignment. so no need to align horizontal.
						align_vertical;

					when CENTER =>
						move_by (border, to_offset (- text_length_half, zero));
						
						align_vertical;
						
					when RIGHT =>
						move_by (border, to_offset (- text_length, zero));
						
						align_vertical;
				end case;
					
				
				-- Rotate as given by argument "rotation":
				rotate_by (border, type_angle (rotation));
				
				-- Mirror if required:
				if mirror = YES then
					mirror_vectors (vectors => border, axis => Y);
				end if;
				
				-- Move to final position as given by argument "position":
				move_by (border, to_offset (position));
			end scale_and_move_border;

			
			-- This procedure merges the given vectorized character
			-- with the result. The result is a collection of lines.
			-- If required by argument make_border, a border around the
			-- character is formed from the list of border vertices:
			procedure add (char : in type_character) is 
				text_lines : pac_character_lines.list := to_lines (char);
				border_vertices : pac_vectors.list;

				use pac_polygons;
				use pac_offsetting;
				p_scratch : type_polygon;
			begin
				scale_and_move_lines (text_lines);
				sorting.merge (target => result.lines, source => text_lines);

				if make_border then
					border_vertices := to_list (char.border);
					scale_and_move_border (border_vertices);
					p_scratch := to_polygon (border_vertices);
					offset_polygon (p_scratch, half_line_width);
					result.borders.append (p_scratch);
				end if;
			end add;

			
			procedure finalize is
				scratch : pac_character_lines.list;

				procedure query_line (c : in pac_character_lines.cursor) is 
					l : type_character_line := element (c);

					procedure align_vertical is begin
						case alignment.vertical is
							when BOTTOM => 
								null; -- text is already computed for bottom alignment. nothing to do
							
							when CENTER =>
								move_by (
									line	=> l,
									offset	=> to_offset (zero, - text_height_half));

								
							when TOP =>
								move_by (
									line	=> l,
									offset	=> to_offset (zero, - text_height));
								
						end case;
					end align_vertical;


					procedure update_text_boundaries is
						sx : constant type_float := get_x (l.start_point);
						sy : constant type_float := get_y (l.start_point);
						ex : constant type_float := get_x (l.end_point);
						ey : constant type_float := get_y (l.end_point);
					begin
						-- update greatest x (right border):
						if sx > result.boundaries.greatest_x then
							result.boundaries.greatest_x := sx;
						end if;

						if ex > result.boundaries.greatest_x then
							result.boundaries.greatest_x := ex;
						end if;

						
						-- update greatest y (upper border):
						if sy > result.boundaries.greatest_y then
							result.boundaries.greatest_y := sy;
						end if;

						if ey > result.boundaries.greatest_y then
							result.boundaries.greatest_y := ey;
						end if;


						-- update smallest x (left border):
						if sx < result.boundaries.smallest_x then
							result.boundaries.smallest_x := sx;
						end if;

						if ex < result.boundaries.smallest_x then
							result.boundaries.smallest_x := ex;
						end if;

						
						-- update smallest y (lower border):
						if sy < result.boundaries.smallest_y then
							result.boundaries.smallest_y := sy;
						end if;

						if ey < result.boundaries.smallest_y then
							result.boundaries.smallest_y := ey;
						end if;
					end update_text_boundaries;

					
				begin -- query_line
					
					-- Align the text with the origin:
					case alignment.horizontal is
						when LEFT => 
							-- text is already computed for left alignment. so no need to align horizontal.
							align_vertical;

						when CENTER =>
							move_by (
								line	=> l,
								offset	=> to_offset (- text_length_half, zero));
							
							align_vertical;
							
						when RIGHT =>
							move_by (
								line	=> l,
								offset	=> to_offset (- text_length, zero));
							
							align_vertical;
					end case;

					
					-- Rotate the text (about the origin) if required:
					if rotation /= zero_rotation then
						rotate_by (l, type_angle (rotation));
					end if;

					-- Mirror the text if required:
					if mirror = YES then
						mirror_line (l, Y);
					end if;
					
					-- Move the line to the given position. 
					-- The given position is the anchor point of the text.
					move_by (l, to_offset (position));					
					
					append (scratch, l);

					-- Update the boundaries of the text by the x/y values
					-- of the current line:
					update_text_boundaries;
				end query_line;

				
			begin -- finalize
				--put_line ("length " & to_string (text_length));
				
				iterate (result.lines, query_line'access);
				result.lines := scratch;

				-- Since the lines have a width, the boundaries must
				-- be extended by half the line width:
				result.boundaries.greatest_x := result.boundaries.greatest_x + half_line_width;
				result.boundaries.greatest_y := result.boundaries.greatest_y + half_line_width;

				result.boundaries.smallest_x := result.boundaries.smallest_x - half_line_width;
				result.boundaries.smallest_y := result.boundaries.smallest_y - half_line_width;
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
					when ':' => add (special_colon);
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


		function first (
			text	: in type_vector_text)
			return pac_character_lines.cursor
		is begin
			return text.lines.first;
		end first;


		procedure iterate (
			text	: in type_vector_text;
			process	: not null access procedure (
				position: in pac_character_lines.cursor))
		is 
			use pac_character_lines;
			c : pac_character_lines.cursor := text.lines.first;
		begin
			while c /= no_element loop
				process (c);				
				next (c);
			end loop;
		end iterate;

		
		function get_lines (
			text	: in type_vector_text)
			return pac_character_lines.list
		is begin
			return text.lines;
		end get_lines;


		function get_borders (
			text	: in type_vector_text)
			return pac_polygons.pac_polygon_list.list
		is begin
			return text.borders;
		end get_borders;
		
		
		function get_linewidth (
			text	: in type_vector_text)
			return type_distance_positive
		is begin 
			return text.width;
		end get_linewidth;
		
		
		function get_boundaries (
			text	: in type_vector_text)
			return pac_geometry_1.type_boundaries
		is begin
			return text.boundaries;
		end get_boundaries;


		procedure update_text_boundaries (
			text	: in out type_vector_text)
		is
			half_line_width : constant type_float_positive := type_float (text.width) * 0.5;
			
			procedure query_line (c : in pac_character_lines.cursor) is
				l : type_character_line renames element (c);
				sx : constant type_float := get_x (l.start_point);
				sy : constant type_float := get_y (l.start_point);
				ex : constant type_float := get_x (l.end_point);
				ey : constant type_float := get_y (l.end_point);
			begin
				-- update greatest x (right border):
				if sx > text.boundaries.greatest_x then
					text.boundaries.greatest_x := sx;
				end if;

				if ex > text.boundaries.greatest_x then
					text.boundaries.greatest_x := ex;
				end if;

				
				-- update greatest y (upper border):
				if sy > text.boundaries.greatest_y then
					text.boundaries.greatest_y := sy;
				end if;

				if ey > text.boundaries.greatest_y then
					text.boundaries.greatest_y := ey;
				end if;


				-- update smallest x (left border):
				if sx < text.boundaries.smallest_x then
					text.boundaries.smallest_x := sx;
				end if;

				if ex < text.boundaries.smallest_x then
					text.boundaries.smallest_x := ex;
				end if;

				
				-- update smallest y (lower border):
				if sy < text.boundaries.smallest_y then
					text.boundaries.smallest_y := sy;
				end if;

				if ey < text.boundaries.smallest_y then
					text.boundaries.smallest_y := ey;
				end if;

			end query_line;
			
		begin
			-- clear the old boundaries:
			text.boundaries := (others => <>);

			-- iterate all line segments:
			text.lines.iterate (query_line'access);

			-- Since the lines have a width, the boundaries must
			-- be extended by half the line width:
			text.boundaries.greatest_x := text.boundaries.greatest_x + half_line_width;
			text.boundaries.greatest_y := text.boundaries.greatest_y + half_line_width;

			text.boundaries.smallest_x := text.boundaries.smallest_x - half_line_width;
			text.boundaries.smallest_y := text.boundaries.smallest_y - half_line_width;
		end update_text_boundaries;


		
		procedure mirror_vector_text (
			text	: in out type_vector_text;
			axis	: in type_axis_2d := Y)
		is
			use pac_polygons;
			result : pac_character_lines.list;
				
			procedure query_line (c : in pac_character_lines.cursor) is
				line : type_character_line := element (c);
			begin
				mirror_line (line, axis);
				result.append (line);
			end query_line;
			
		begin
			-- line segments:
			text.lines.iterate (query_line'access);
			text.lines := result;

			-- borders
			mirror_polygons (text.borders, axis);

			-- boundaries
			update_text_boundaries (text);
		end mirror_vector_text;


		procedure rotate_vector_text (
			text	: in out type_vector_text;
			angle	: in type_rotation)
		is
			angle_float : constant type_angle := type_angle (angle);
			
			use pac_polygons;
			result : pac_character_lines.list;
				
			procedure query_line (c : in pac_character_lines.cursor) is
				line : type_character_line := element (c);
			begin
				rotate_by (line, angle_float);
				result.append (line);
			end query_line;
			
		begin
			-- line segments:
			text.lines.iterate (query_line'access);
			text.lines := result;

			-- borders
			rotate_polygons (text.borders, angle_float);

			-- boundaries
			update_text_boundaries (text);
		end rotate_vector_text;


		procedure move_vector_text (
			text	: in out type_vector_text;
			offset	: in type_distance_relative)
		is
			offset_float : constant type_offset := to_offset (offset);
			
			use pac_polygons;
			result : pac_character_lines.list;
				
			procedure query_line (c : in pac_character_lines.cursor) is
				line : type_character_line := element (c);
			begin
				move_by (line, offset_float);
				result.append (line);
			end query_line;
			
		begin
			-- line segments:
			text.lines.iterate (query_line'access);
			text.lines := result;

			-- borders
			move_polygons (text.borders, offset_float);

			-- boundaries
			update_text_boundaries (text);
		end move_vector_text;

		
	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
