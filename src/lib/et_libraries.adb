------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        COMPONENT LIBRARIES                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_string_processing;
with et_coordinates;
with et_import;
with et_text;

with et_devices;				use et_devices;

package body et_libraries is


	
	
	function to_string (person : in type_person_name.bounded_string) return string is
	-- Returns the given person name as string.
	begin
		return latin_1.space & type_person_name.to_string (person);
	end to_string;

	function to_string ( -- CS remove
		size		: in pac_text.type_text_size;
		preamble	: in boolean := true) return string is
	-- Returns the given text size as string.
	begin
		if preamble then
			return "size " & geometry.to_string (size);
		else
			return geometry.to_string (size);
		end if;
	end to_string;






	










	
	
	

	

	





	
	function to_string (purpose : in type_device_purpose.bounded_string) return string is
	-- Returns the given purpose as string.
	begin
		return type_device_purpose.to_string (purpose);
	end to_string;

	function purpose_length_valid (purpose : in string) return boolean is 
	-- Returns true if given purpose is too long. Issues warning.
		use et_string_processing;
	begin
		if purpose'length > device_purpose_length_max then
			log (WARNING, "purpose " & enclose_in_quotes (purpose) & " is longer than" 
				 & positive'image (device_purpose_length_max) & " characters !", 
				console => true);
			return false;
		else
			return true;
		end if;
	end;
		
	function purpose_characters_valid (
	-- Tests if the given value contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
		purpose		: in type_device_purpose.bounded_string;
		characters	: in character_set := device_purpose_characters) 
		return boolean is
		use et_string_processing;
		use type_device_purpose;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> purpose,
			set 	=> characters,
			test 	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "purpose " & enclose_in_quotes (to_string (purpose))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position)
				);
			return false;
		else
			return true;
		end if;
	end purpose_characters_valid;

	procedure purpose_invalid (purpose : in string) is 
	-- Issues error message and raises constraint error.
		use et_string_processing;
	begin
		log (ERROR, "purpose " & enclose_in_quotes (purpose) &
			 " invalid !", console => true);
		raise constraint_error;
	end;

	function to_purpose (
	-- Tests the given purpose for length and invalid characters.
		purpose 					: in string;
		error_on_invalid_character	: in boolean := true)
		return type_device_purpose.bounded_string is

		purpose_out : type_device_purpose.bounded_string; -- to be returned
	begin
		-- Test length of given purpose:
		if purpose_length_valid (purpose) then
			purpose_out := type_device_purpose.to_bounded_string (purpose);
		else
			purpose_invalid (purpose);
		end if;

		-- Test characters:
		if purpose_characters_valid (purpose_out) then
			null;
		else
			purpose_invalid (purpose);
		end if;

		return purpose_out;
	end to_purpose;
	


	function to_string (index : in type_device_name_index) return string is begin
		return latin_1.space & trim (type_device_name_index'image (index), left);
	end to_string;

	function to_device_name_index (index : in string) return type_device_name_index is begin
		return type_device_name_index'value (index);
	end to_device_name_index;

	function to_device_name (
	-- Converts a string like "IC303" to a composite type_device_name.
	-- Raises constraint error if prefix contains invalid characters.
	-- Raises constraint error if id contains non-digit characters.
	-- Leading zeroes in the id are removed. R002 becomes R2.
		text_in : in string)
		return type_device_name is
		use et_libraries;

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := text_in;
	
		r : type_device_name := (
				prefix		=> type_device_name_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : type_device_name_prefix.bounded_string;
	
		procedure invalid_reference is
			use et_string_processing;
		begin
			log (ERROR, latin_1.lf & "invalid device name '" 
				 & text_in_justified & "'", console => true);
			-- CS show position of affected character ?
			raise constraint_error;
		end invalid_reference;

		d : positive;
		digit : natural := 0;

		use et_libraries.type_device_name_prefix;

	begin -- to_device_name
		-- assemble prefix
		for i in text_in_justified'first .. text_in_justified'last loop
			c := text_in_justified(i);
			
			case i is 
				-- The first character MUST be a valid prefix character.
				when 1 => 
					if is_in (c, device_name_prefix_characters) then
						r.prefix := r.prefix & c;
					else 
						invalid_reference;
					end if;
					
				-- Further characters are appended to prefix if they are valid prefix characters.
				-- If anything else is found, the prefix is assumed as complete.
				when others =>
					if is_in (c, device_name_prefix_characters) then
						r.prefix := r.prefix & c;
					else
						d := i; -- d holds the position of the charcter after the prefix.
							-- d is requried when reading the component id. see below.
						exit;
					end if;
			end case;
		end loop;

		-- assemble id
		-- Start with the last character in text_in_justified.
		-- Finish at the position d (that is the first digit after the last letter, see above).
		-- All the characters within this range must be digits.
		-- The significance of the digit is increased after each pass.
		for i in reverse d .. text_in_justified'last loop
			c := text_in_justified(i);
			
			if is_digit(c) then
				r.id := r.id + 10**digit * natural'value(1 * c);
			else
				invalid_reference;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;

		-- Set the id width.
		-- It is the number of digits processed when the id was assembled (see above).
		-- Example: if the given string was IC002 then digit is 3.
		r.id_width := digit;
		
		return r;
	end to_device_name;

	function "<" (left, right : in type_device_name) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use type_device_name_prefix;
	begin
		-- First we compare the prefix.
		-- Example: If left is C201 and right is R4 then the result is true as C comes before R.

		if left.prefix < right.prefix then -- like C201 and R4
			result := true;
		elsif left.prefix > right.prefix then -- like R4 and C201
			result := false;
		elsif left.prefix = right.prefix then -- like IC33 and IC34

			-- If equal prefixes, we compare the id:
			if left.id < right.id then -- like 33 and 34
				result := true;
			else
				result := false; -- like 34 and 33
			end if;

		end if;

		-- in case of equivalence of left and right, we return false (default)
		return result;
	end;	

	function equal_name (left, right : in type_device_name) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use et_libraries;
		use et_libraries.type_device_name_prefix;
	begin
		-- First we compare the prefix. If prefixes are equal, we compare the id.
		-- If either of them does not match, the result is set false.
		if left.prefix = right.prefix then -- like IC and IC

			if left.id = right.id then -- like 4 and 4
				result := true;
			else -- like 5 and 6
				result := false;
			end if;
			
		else -- like R and IC
			result := false; 
		end if;

		return result;
	end;

	function to_string (name : in type_device_name) return string is
	-- Returns the given device name as string.
	-- Prepends leading zeros according to name.id_width.
		id_width_wanted	: natural := name.id_width;
	
		-- The width of the given id is obtained by converting the id to a string
		-- and then by measuring its length:
		id_width_given	: natural := trim (natural'image (name.id),left)'length;

		-- Finally the number of zeros to prepend is the difference of wanted 
		-- and given digits:
		lz : natural := id_width_wanted - id_width_given;
	begin
		case lz is
			when 0 => -- no leading zeroes
				return type_device_name_prefix.to_string (name.prefix) 
					& trim (natural'image (name.id),left);
				
			when others => -- leading zeros required
				return type_device_name_prefix.to_string (name.prefix) 
					& lz * '0' & trim (natural'image (name.id),left);
		end case;
	end to_string;
	
	function prefix (name : in type_device_name) return type_device_name_prefix.bounded_string is begin
	-- Returns the prefix of the given device name.
		return name.prefix;
	end;

	function index (name : in type_device_name) return type_device_name_index is begin
	-- Returns the index of the given device name.
		return name.id;
	end;

	function to_device_name (
	-- Builds a device name by given prefix (like R) and index (like 23) to a device name (like R23).
	-- If width is not provided, then the width of the index is calculated automatically. In case of R23 the width is 2.
	-- If width is provided, then it is set accordingly.
		prefix	: in type_device_name_prefix.bounded_string; 	-- R, C, L
		index	: in type_device_name_index;					-- 1, 20, ..
		width	: in type_device_name_index_width := type_device_name_index_width'first) -- the number of digits
		return type_device_name is
		device_name : type_device_name; -- to be returned
	begin
		-- assign prefix and index as requested:
		device_name.prefix := prefix;
		device_name.id := index;

		-- Calculate the width of the index. examples: it is 3 for IC987, 2 for C77
		-- The width of the index is obtained by converting the given index to a string
		-- and then by measuring its length:
		device_name.id_width := trim (natural'image (index),left)'length;

		-- If width IS provided AND wider than the just calculated width,
		-- then the calculated width is overwritten.
		if width /= type_device_name_index_width'first then

			-- If width is smaller or equal the calculated width nothing happens.
			-- Otherwise width is set according to the provided width:
			if width <= device_name.id_width then
				null;
			else			
				device_name.id_width := width;
			end if;
		end if;
		
		return device_name;
	end; -- to_device_name

	procedure offset_device_name (
	-- Adds to the device index the given offset. 
	-- Example: given name is R4, given offset is 100. Result R104.
		name	: in out type_device_name;
		offset	: in type_device_name_index) is
	begin
		name := to_device_name (
			prefix	=> prefix (name),
			index	=> name.id + offset);
		-- the width of the index is calculated automatically by to_device_name.
		
	end offset_device_name;








	
	
	
	function to_string (packge : in type_component_package_name.bounded_string) return string is
	-- Returns the given package name as string.
	-- CS: provide a parameter that turns the preamble on/off
	begin
		return type_component_package_name.to_string (packge);
	end to_string;

	function to_package_name (package_name : in string) return type_component_package_name.bounded_string is
	-- Converts a string to a type_component_package_name.	
	begin
		return type_component_package_name.to_bounded_string (package_name);
	end to_package_name;
	
	procedure check_package_name_length (packge : in string) is
	-- Tests if the given package is longer than allowed.
		use et_string_processing;
	begin
		if packge'length > component_package_name_length_max then
			log (WARNING, "package name too long. Max. length is" 
				 & positive'image (component_package_name_length_max) & " !");
		end if;
	end check_package_name_length;

	procedure check_package_name_characters (
		packge		: in type_component_package_name.bounded_string;
		characters	: in character_set := component_package_name_characters)
		is
	-- Tests if the given package name contains only valid characters as specified
	-- by given character set.
	-- Raises exception if invalid character found.
		use et_string_processing;
		use type_component_package_name;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source => packge,
			set => characters,
			test => outside);

		if invalid_character_position > 0 then
			log (WARNING, "package name " & to_string (packge) 
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
		end if;
	end check_package_name_characters;

	

	


	function to_string (name : in type_frame_template_name.bounded_string) return string is begin
		return type_frame_template_name.to_string (name);
	end to_string;
	
	function to_template_name (name : in string) return type_frame_template_name.bounded_string is begin
		return type_frame_template_name.to_bounded_string (name);
	end to_template_name;
	


	
	
end et_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
