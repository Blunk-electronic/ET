------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              MATERIAL                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
with ada.directories;
with gnat.directory_operations;
with ada.exceptions;

with et_general;				use et_general;
with assembly_variants;
with et_string_processing;		use et_string_processing;
with et_export;
with et_csv;					use et_csv;
with et_packages;
with et_devices;				use et_devices;


package body material is

	function to_string (partcode : in type_partcode.bounded_string) return string is begin
		return type_partcode.to_string (partcode);
	end to_string;
	
	function partcode_length_valid (partcode : in string) return boolean is
		-- Returns true if length of given partcode is ok. Issues warning if not.	
		use et_string_processing;
	begin
		if partcode'length > partcode_length_max then
			log (WARNING, "partcode " & enclose_in_quotes (partcode) & " is longer than" 
				 & positive'image (partcode_length_max) & " characters !");
			return false;
		else
			return true;
		end if;
	end;

	function partcode_characters_valid (
		partcode	: in type_partcode.bounded_string;
		characters	: in character_set := partcode_characters) return boolean is
	-- Tests if the given partcode contains only valid characters as specified
	-- by given character set. Returns false if not. Issues warning.
		use et_string_processing;
		use type_partcode;
		invalid_character_position : natural := 0;
	begin
		invalid_character_position := index (
			source	=> partcode,
			set		=> characters,
			test	=> outside);

		if invalid_character_position > 0 then
			log (WARNING, "partcode " & enclose_in_quotes (to_string (partcode))
				 & " has invalid character at position"
				 & natural'image (invalid_character_position));
			return false;
		else
			return true;
		end if;
	end;

	procedure partcode_invalid (partcode : in string) is 
	-- Issues error message and raises constraint error.
		use et_string_processing;
	begin
		log (ERROR, "partcode " & enclose_in_quotes (partcode) &
			 " invalid !", console => true);
		raise constraint_error;
	end;

	function to_partcode (
	-- Tests the given value for length and invalid characters.							 
		partcode 					: in string;
		error_on_invalid_character	: in boolean := true) 
		return type_partcode.bounded_string is

		partcode_out : type_partcode.bounded_string; -- to be returned
	begin
		-- Test length of given partcode
		if partcode_length_valid (partcode) then
			partcode_out := type_partcode.to_bounded_string (partcode);
		else
			partcode_invalid (partcode);
		end if;

		-- Test characters
		if partcode_characters_valid (partcode_out) then
			null;
		else
			partcode_invalid (partcode);
		end if;

		return partcode_out;
	end to_partcode;

	
	function to_string (name : in type_file_name.bounded_string) return string is begin
		return type_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return type_file_name.bounded_string is begin
		return type_file_name.to_bounded_string (name);
	end;

	procedure write_bom (
	-- Creates the BOM file (which inevitably and intentionally overwrites the previous file).
	-- Writes the content of the given container bom in the file.
	-- - The BOM file will be named after the module name and the assembly variant.
	-- - Exports the BOM of the given module to the export/CAM/BOM directory.
		bom				: in type_devices.map;
		module_name		: in type_module_name.bounded_string; -- motor_driver 
		variant_name	: in type_variant_name.bounded_string; -- low_cost
		format			: in type_bom_format;
		log_threshold	: in type_log_level) is		

		file_name : type_file_name.bounded_string;
		
		procedure set_file_name is 
			use ada.directories;
			use gnat.directory_operations;
			use type_module_name;
			use et_general.type_variant_name;
			use et_export;
		begin
			if is_default (variant_name) then
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_bom,

								name					=> et_general.to_string (module_name),
								extension				=> extension_bom
							));

			else
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_bom,

								name					=> et_general.to_string (module_name) & "_" & 
															to_variant (variant_name),
								extension				=> extension_bom
							));
			end if;
		end;	
		
		bom_handle : ada.text_io.file_type;
		device_cursor : type_devices.cursor := bom.first;

		procedure eagle is
			use et_csv;
			
			column_part			: constant string := "Part";
			column_value		: constant string := "Value";
			column_device		: constant string := "Device";
			column_package		: constant string := "Package";
			column_description	: constant string := "Description";
			column_bom			: constant string := "BOM";
			column_commissioned	: constant string := "COMMISSIONED";
			column_function		: constant string := "FUNCTION";
			column_partcode		: constant string := "PART_CODE_BEL"; -- CS: should be PART_CODE in the future. requires changing stock_manager
			column_partcode_ext	: constant string := "PART_CODE_EXT"; -- not used
			column_updated		: constant string := "UPDATED";

			procedure query_device (cursor : in type_devices.cursor) is
				use type_devices;
				use et_packages;
			begin
				put_field (file => bom_handle, text => to_string (key (cursor))); -- R4
				put_field (file => bom_handle, text => to_string (element (cursor).value)); -- 100R
				put_field (file => bom_handle); -- empty generic device name, doesn't matter any more
				put_field (file => bom_handle, text =>
					ada.directories.base_name (to_string (element (cursor).packge))); -- S_0805
				put_field (file => bom_handle); -- empty description
				put_field (file => bom_handle, text => "YES"); -- BOM status
				put_field (file => bom_handle); -- empty commission date, doesn't matter any more
				put_field (file => bom_handle, text => to_string (element (cursor).purpose)); -- purpose/function
				put_field (file => bom_handle, text => to_string (element (cursor).partcode));
				put_field (file => bom_handle); -- empty external partcode, never used
				put_field (file => bom_handle); -- empty update date, doesn't matter any more

				put_lf (file => bom_handle, field_count => et_csv.column);
			end query_device;
			
		begin -- eagle
			-- CS: A nice header should be placed. First make sure stock_manager can handle it.
			
			-- write the BOM table header
			et_csv.reset_column;
			put_field (file => bom_handle, text => column_part);
			put_field (file => bom_handle, text => column_value);
			put_field (file => bom_handle, text => column_device);
			put_field (file => bom_handle, text => column_package);
			put_field (file => bom_handle, text => column_description);
			put_field (file => bom_handle, text => column_bom);
			put_field (file => bom_handle, text => column_commissioned);
			put_field (file => bom_handle, text => column_function);
			put_field (file => bom_handle, text => column_partcode);
			put_field (file => bom_handle, text => column_partcode_ext);
			put_field (file => bom_handle, text => column_updated);
			put_lf    (file => bom_handle, field_count => et_csv.column);

			type_devices.iterate (
				container	=> bom,
				process		=> query_device'access);
			
			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
			-- put_line (bom_handle, comment_mark & " end of list");
			
		end;

		procedure native is -- CS not complete. accessories ?
			use et_csv;

			column_item			: constant string := "ITEM";
			column_device		: constant string := "DEVICE";			
			column_value		: constant string := "VALUE";
			column_package		: constant string := "PACKAGE";
			column_partcode		: constant string := "PARTCODE";
			column_purpose		: constant string := "PURPOSE";

			procedure query_device (cursor : in type_devices.cursor) is
				use type_devices;
				use et_packages;
			begin
				put_field (file => bom_handle); -- CS item number
				put_field (file => bom_handle, text => to_string (key (cursor))); -- R4
				put_field (file => bom_handle, text => to_string (element (cursor).value)); -- 100R
				put_field (file => bom_handle, text => to_string (element (cursor).packge)); -- S_0805.pac
				put_field (file => bom_handle, text => to_string (element (cursor).partcode)); -- R_PAC_S_0805_VAL_100R
				put_field (file => bom_handle, text => to_string (element (cursor).purpose)); -- purpose

				put_lf (file => bom_handle, field_count => et_csv.column);
			end query_device;
			
		begin -- native
			-- CS: A nice header should be placed. First make sure stock_manager can handle it.
			
			-- write the BOM table header
			et_csv.reset_column;
			put_field (file => bom_handle, text => column_item);
			put_field (file => bom_handle, text => column_device);
			put_field (file => bom_handle, text => column_value);
			put_field (file => bom_handle, text => column_package);
			put_field (file => bom_handle, text => column_partcode);
			put_field (file => bom_handle, text => column_purpose);
			put_lf    (file => bom_handle, field_count => et_csv.column);

			type_devices.iterate (
				container	=> bom,
				process		=> query_device'access);
			
			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
			-- put_line (bom_handle, comment_mark & " end of list");
			
		end;

		
	begin -- write_bom
		-- build the name of the BOM file
		set_file_name;

		log (text => "writing BOM file " & enclose_in_quotes (to_string (file_name)) & " ...", level => log_threshold);
		
		create (
			file => bom_handle,
			mode => out_file, 
			name => to_string (file_name));

		case format is
			when NATIVE => native;
			when EAGLE => eagle;
			when others => raise constraint_error;
		end case;
				
		close (bom_handle);

		exception
			when event: others =>
				if is_open (bom_handle) then
					close (bom_handle);
				end if;
				
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
				
	end write_bom;

	
end material;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
