------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
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
with ada.strings;				use ada.strings;
with ada.characters.handling;	use ada.characters.handling;

with et_exceptions;				use et_exceptions;


package body et_text is

	
	
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





		procedure reset_text (
			text : in out type_text)
		is begin
			text.size := size_default;
			text.alignment := text_alignment_default;
			reset_status (text.status);
		end;
		


		


		function is_proposed (
			text : in type_text)
			return boolean
		is begin
			if is_proposed (text.status) then
				return true;
			else
				return false;
			end if;
		end is_proposed;
			

		procedure set_proposed (
			text : in out type_text)
		is begin
			set_proposed (text.status);
		end set_proposed;
		

		procedure clear_proposed (
			text : in out type_text)
		is begin
			clear_proposed (text.status);
		end clear_proposed;
		

		
		function is_moving (
			text : in type_text)
			return boolean
		is begin
			if is_moving (text.status) then
				return true;
			else
				return false;
			end if;
		end is_moving;
		

		
		procedure set_moving (
			text : in out type_text)
		is begin
			set_moving (text.status);
		end set_moving;

		

		procedure clear_moving (
			text : in out type_text)
		is begin
			clear_moving (text.status);
		end clear_moving;
		

		function is_selected (
			text : in type_text)
			return boolean
		is begin
			if is_selected (text.status) then
				return TRUE;
			else
				return FALSE;
			end if;
		end is_selected;
		

		procedure set_selected (
			text : in out type_text)
		is begin
			set_selected (text.status);
		end set_selected;
		

		procedure clear_selected (
			text : in out type_text)
		is begin
			clear_selected (text.status);
		end clear_selected;

		

		procedure modify_status (
			text 		: in out type_text;
			operation	: in et_object_status.type_status_operation)
		is begin
			modify_status (text.status, operation);
		end modify_status;


		

		procedure reset_status (
			text 		: in out type_text)
		is begin
			reset_status (text.status);
		end reset_status;

		
		

		
		function text_properties (
			text : in type_text) 
			return string 
		is begin
			return
				"size" & to_string (text.size)
				& to_string (text.alignment);
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

		
		
		
		function to_rotation_doc (
			rotation : in type_rotation) 
			return type_rotation_documentation
		is
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


		
		
		function to_rotation_doc (
			rotation : in string) 
			return type_rotation_documentation 
		is
			r : constant type_rotation := to_rotation (rotation);
		begin
			if r = zero_rotation then
				return HORIZONTAL;
				
			elsif r = 90.0 then
				return VERTICAL;
				
			else
				warning_rotation_outside_range;
				return to_rotation_doc (r);
			end if;
		end;
	

		
	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
