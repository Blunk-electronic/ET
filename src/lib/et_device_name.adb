------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICES NAMES                                  --
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

with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.handling;	use ada.characters.handling;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_device_name is

	use pac_device_prefix;
	

	function to_string (index : in type_name_index) 
		return string 
	is begin
		return latin_1.space & trim (type_name_index'image (index), left);
	end;



	function to_index (index : in string) return type_name_index is begin
		return type_name_index'value (index);
	end;


	
	function same_prefix (
		left, right : in type_device_name) 
		return boolean 
	is 
		use pac_device_prefix;
	begin
		if get_prefix (left) = get_prefix (right) then
			return true;
		else
			return false;
		end if;
	end same_prefix;

	
	
	function to_device_name (text_in : in string) return type_device_name is

		-- justify given text_in on the left
		text_in_justified : string (1 .. text_in'length) := to_upper (text_in);
	
		r : type_device_name := (
				prefix		=> pac_device_prefix.to_bounded_string(""),
				id 			=> 0,
				id_width	=> 1);
	
		c : character;
		p : pac_device_prefix.bounded_string;
	
		procedure invalid_device_name is
		begin
			--log (ERROR, latin_1.lf & "invalid device name '" 
				 --& text_in_justified & "'", console => true);

			-- CS show position of affected character ?
			raise syntax_error_1 with 
				"ERROR: Device name " & enclose_in_quotes (text_in_justified) & " invalid !";

		end invalid_device_name;

		d : positive;
		digit : natural := 0;

		use pac_device_prefix;

		
	begin -- to_device_name
		-- assemble prefix
		for i in text_in_justified'first .. text_in_justified'last loop
			c := text_in_justified (i);
			
			case i is 
				-- The first character MUST be a valid prefix character.
				when 1 => 
					if is_in (c, prefix_characters) then
						r.prefix := r.prefix & c;
					else 
						invalid_device_name;
					end if;
					
				-- Further characters are appended to prefix if they are valid prefix characters.
				-- If anything else is found, the prefix is assumed as complete.
				when others =>
					if is_in (c, prefix_characters) then
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
			c := text_in_justified (i);
			
			if is_digit (c) then
				r.id := r.id + 10**digit * natural'value (1 * c);
			else
				invalid_device_name;
			end if;

			digit := digit + 1; -- increase digit significance (10**0, 10**1, ...)
		end loop;
		
		-- There must be at least one digit. If no digit found, then the given
		-- device name has no index.
		if digit > 0 then
		
			-- Set the id width.
			-- It is the number of digits processed when the id was assembled (see above).
			-- Example: if the given string was IC002 then digit is 3.
			r.id_width := digit;
		else
			raise syntax_error_1 with "ERROR: Device name invalid. Missing index !";
		end if;
		
		return r;
	end to_device_name;

	

	
	function "<" (left, right : in type_device_name) return boolean is
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.
	-- CS: needs verification !
		result : boolean := false;
		use pac_device_prefix;
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



	
	function "=" (left, right : in type_device_name) return boolean is
	-- Returns true if left equals right.
	-- Example: if IC4 = IC4 then return true.
		result : boolean := false;
		use pac_device_prefix;
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
				return pac_device_prefix.to_string (name.prefix) 
					& trim (natural'image (name.id),left);
				
			when others => -- leading zeros required
				return pac_device_prefix.to_string (name.prefix) 
					& lz * '0' & trim (natural'image (name.id),left);
		end case;
	end to_string;


	
	function get_prefix (
		name : in type_device_name) 
		return pac_device_prefix.bounded_string 
	is begin
		return name.prefix;
	end;


	
	function get_index (
		name : in type_device_name) 
		return type_name_index 
	is begin
		return name.id;
	end;


	
	function to_device_name (
		prefix	: in pac_device_prefix.bounded_string; 	-- R, C, L
		index	: in type_name_index;				-- 1, 20, ..
		width	: in type_index_width := type_index_width'first) -- the number of digits
		return type_device_name 
	is
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
		if width /= type_index_width'first then

			-- If width is smaller or equal the calculated width nothing happens.
			-- Otherwise width is set according to the provided width:
			if width <= device_name.id_width then
				null;
			else			
				device_name.id_width := width;
			end if;
		end if;
		
		return device_name;
	end;


	
	procedure offset_index (
		name	: in out type_device_name;
		offset	: in type_name_index) 
	is begin
		name := to_device_name (
			prefix	=> get_prefix (name),
			index	=> name.id + offset);
		-- the width of the index is calculated automatically by to_device_name.
	end;


	
end et_device_name;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
