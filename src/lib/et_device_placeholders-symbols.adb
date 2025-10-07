------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN SYMBOLS                         --
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
--                                                                          --
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


with et_alignment;				use et_alignment;


package body et_device_placeholders.symbols is


	function in_catch_zone (
		placeholder		: in type_text_placeholder;
		unit_position	: in type_vector_model;					   
		zone			: in type_catch_zone)
		return boolean
	is 
		point : type_vector_model := placeholder.position;
	begin
		move_by (point, unit_position);
		
		-- put_line ("pos " & to_string (point));
		
		if in_catch_zone (zone, point) then
			return true;
		else
			return false;
		end if;
	end in_catch_zone;




	procedure reset_status (
		placeholders : in out type_default_placeholders)
	is begin
		reset_status (placeholders.name);
		reset_status (placeholders.value);
		reset_status (placeholders.purpose);
	end;
	




	

	procedure rotate_placeholders (
		placeholders	: in out type_default_placeholders;
		rotation		: in type_rotation_model)
	is begin
		-- Rotate the POSITIONS	of the placeholders about
		-- the origin of the symbol:
		rotate_by (placeholders.name.position, rotation);
		rotate_by (placeholders.value.position, rotation);
		rotate_by (placeholders.purpose.position, rotation);

		-- Rotate the placeholders about THEIR OWN ORIGIN.
		-- The resulting angle is the sum of the initial 
		-- rotation (given by the symbol model) and the rotation
		-- of the unit.
		-- After summing up the rotation must be snapped to either
		-- HORIZONTAL or VERTICAL so that the text is readable
		-- from the right or from the front of the drawing.
		placeholders.name.rotation := 
			to_rotation_doc (to_rotation (placeholders.name.rotation) + rotation);

		placeholders.value.rotation := 
			to_rotation_doc (to_rotation (placeholders.value.rotation) + rotation);

		placeholders.purpose.rotation := 
			to_rotation_doc (to_rotation (placeholders.purpose.rotation) + rotation);
	end rotate_placeholders;





	function rotate_placeholders (
		placeholders	: in type_default_placeholders;
		rotation		: in type_rotation_model)
		return type_default_placeholders
	is
		result : type_default_placeholders := placeholders;
	begin
		rotate_placeholders (result, rotation);
		return result;
	end;
	

	
	
	
	procedure write_placeholder_properties (
		placeholder		: in type_text_placeholder;
		log_threshold	: in type_log_level) 
	is begin
		-- meaning
		log (text => to_string (placeholder.meaning), level => log_threshold);
		log_indentation_up;
		
		-- position
		log (text => to_string (placeholder.position), level => log_threshold);

		-- size
		log (text => to_string (placeholder.size), level => log_threshold);

		-- rotation
		log (text => to_string (placeholder.rotation), level => log_threshold); 

		-- visible
		--log (text => "visible "
		--	& to_lower (et_libraries.type_text_visible'image (placeholder.visible)), level => log_threshold);

		-- alignment
		log (text => to_string (placeholder.alignment),
			level => log_threshold);

		log_indentation_down;
	end write_placeholder_properties;

	
end et_device_placeholders.symbols;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
