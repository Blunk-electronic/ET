------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NET NAMES                                   --
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

with ada.strings.fixed; 		use ada.strings.fixed;


package body et_net_names is


	function net_name_to_string (
		net_name	: in pac_net_name.bounded_string)
		return string
	is begin
		return pac_net_name.to_string (net_name);
	end net_name_to_string;


	
	
	procedure check_net_name_length (net : in string) is begin
		if net'length > net_name_length_max then
			log (ERROR, "max. number of characters for net name is" 
				 & positive'image (net_name_length_max) & " !",
				 console => true);
			raise constraint_error;
		end if;
	end check_net_name_length;



	
	procedure check_net_name_characters (
		net			: in pac_net_name.bounded_string;
		characters	: in character_set := net_name_characters) 
	is
		invalid_character_position : natural := 0;
		inversion_mark_position : natural := 0;
	begin
		-- Test given net name and get position of possible invalid characters.
		invalid_character_position := index (
			source => net,
			set => characters,
			test => outside);

		-- Evaluate position of invalid character.
		if invalid_character_position > 0 then
			log (ERROR, "invalid character in net name '" 
				 & to_string (net) & "' at position" 
				 & natural'image (invalid_character_position) & " !",
				 console => true);

			-- CS: show allowed characters
			raise constraint_error;
		end if;

		-- If there is an inversion mark, it must be at the very end of the net name.
		inversion_mark_position := pac_net_name.index (net, net_inversion_mark);
		if inversion_mark_position > 0 then
			if inversion_mark_position /= pac_net_name.length (net) then
				log (ERROR, "net " & to_string (net) 
					& " inversion mark must be at the end of the net name !",
					console => true);
				raise constraint_error;
			end if;
		end if;
		
	end check_net_name_characters;



	
	function is_empty (net : in pac_net_name.bounded_string)
		return boolean
	is begin
		if length (net) = 0 then
			return true;
		else
			return false;
		end if;
	end is_empty;



	
	function to_net_name (net_name : in string) return pac_net_name.bounded_string is begin
		return pac_net_name.to_bounded_string (to_upper (net_name));
	end to_net_name;
	



	
	function to_anonymous_net_name (index : in type_anonymous_net_index) -- 56
		return pac_net_name.bounded_string -- N$56
	is
		name : constant string := anonymous_net_name_prefix -- N$
			& trim (type_anonymous_net_index'image (index), left); -- 56
	begin
		return to_net_name (name); -- N$56
	end to_anonymous_net_name;




	
	function anonymous (net_name : in pac_net_name.bounded_string) -- N$456
		return boolean 
	is
		result : boolean := true;

		-- Extract the prefix. It should be "N$":
		prefix : constant string := slice (net_name, 1, 2);

		-- Extract the index. It should be a number like "456"
		index_string : constant string := slice (net_name, 3, length (net_name));
	begin
		-- If the net name does not start with N$ then it is not an anonymous net.
		-- Otherwise the trailing characters must be investigated:
		if prefix = anonymous_net_name_prefix then

			-- Could be an anonymous net. Check the index_string. 
			-- If index_string contains something that is not a digit,
			-- then it is not an anonymous net. Abort loop prematurely:			
			for c in index_string'first .. index_string'last loop
				if not is_digit (index_string (c)) then
					result := false;
					exit;
				end if;
			end loop;

			-- Result is still true if index_string contains only digits.
			
		else 
			result := false; -- not anonymous
		end if;

		return result;
	end anonymous;



	

	procedure clear_net_name (
		net_name : in out pac_net_name.bounded_string)
	is begin
		net_name := no_name;
	end;

	
	

	function to_string (
		net	: in pac_net_names.cursor)
		return string
	is
		use pac_net_names;
	begin
		return pac_net_name.to_string (element (net));
	end to_string;


	
	
end et_net_names;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
