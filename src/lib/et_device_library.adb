------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           DEVICE LIBRARY                                 --
--                                                                          --
--                              B o d y                                     --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_device_library is




	--function hash_device_model (
		--model	: in pac_device_model_file.bounded_string)
		--return hash_type
	--is 
	--begin
		--return ada.strings.hash (to_string (model));
	--end hash_device_model;


	--function equivalent_models (
		--d1, d2 : in type_device_model)
		--return boolean
	--is begin
		--return d1 = d2;
	--end equivalent_models;


	function get_prefix (
		cursor	: in pac_devices_lib.cursor)
		return pac_device_prefix.bounded_string
	is begin
		return element (cursor).prefix;
	end;


	

	function get_device_model_file (
		cursor	: in pac_devices_lib.cursor)
		return pac_device_model_file.bounded_string
	is begin
		return key (cursor);
	end;




	procedure create_device (
		device_name		: in pac_device_model_file.bounded_string;
		appearance		: in type_appearance;
		log_threshold	: in type_log_level) 
	is
		use et_string_processing;
		use pac_devices_lib;
	begin
		log (text => "creating device " & to_string (device_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "appearance " & to_string (appearance) & " ...", level => log_threshold);
		
		-- Test if device already exists. If already exists, issue warning and exit.
		if contains (device_library, device_name) then
			log (WARNING, text => "device already exists -> skipped", level => log_threshold + 1);
		else
			case appearance is
				when APPEARANCE_PCB =>
					insert (
						container	=> device_library,
						key			=> device_name,
						new_item	=> (appearance => APPEARANCE_PCB, others => <>)
						);

				when APPEARANCE_VIRTUAL =>
					insert (
						container	=> device_library,
						key			=> device_name,
						new_item	=> (appearance => APPEARANCE_VIRTUAL, others => <>)
						);
			end case;					
		end if;

		log_indentation_down;
	end create_device;



	

	function get_device_model (
		model : in pac_device_model_file.bounded_string)
		return pac_devices_lib.cursor 
	is
		cursor : pac_devices_lib.cursor := find (device_library, model);
	begin
		return cursor;
	end;

	

	

	
	function is_real (
		device_cursor : in pac_devices_lib.cursor)
		return boolean
	is begin
		case element (device_cursor).appearance is
			when APPEARANCE_VIRTUAL => return false;
			when APPEARANCE_PCB => return true;
		end case;
	end is_real;

	


	




	function get_default_value (
		device_cursor : in pac_devices_lib.cursor)
		return pac_device_value.bounded_string
	is
		device_model : type_device_model renames element (device_cursor);
	begin
		return get_default_value (device_model);
	end;


	
		
end et_device_library;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
