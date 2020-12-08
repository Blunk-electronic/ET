------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PICK AND PLACE                                --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
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

with ada.strings; 				use ada.strings;
with ada.directories;
with gnat.directory_operations;
with ada.exceptions;

with et_export;
with et_csv;					use et_csv;


package body et_pick_and_place is
	
	function to_string (name : in type_file_name.bounded_string) return string is begin
		return type_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return type_file_name.bounded_string is begin
		return type_file_name.to_bounded_string (name);
	end;

	procedure write_pnp (
		pnp				: in type_devices.map;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in et_general.pac_assembly_variant_name.bounded_string; -- low_cost
		format			: in type_pnp_format := NATIVE;
		log_threshold	: in type_log_level) is		

		file_name : type_file_name.bounded_string;
		
		pnp_handle : ada.text_io.file_type;
		device_cursor : type_devices.cursor := pnp.first;

		procedure set_file_name is 
			use ada.directories;
			use gnat.directory_operations;
			use pac_module_name;
			use et_general.pac_assembly_variant_name;
			use et_export;
		begin
			if is_default (variant_name) then
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_pick_and_place,

								name					=> et_general.to_string (module_name),
								extension				=> extension_pnp
							));

			else
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_pick_and_place,

								name					=> et_general.to_string (module_name) & "_" & 
															to_variant (variant_name),
								extension				=> extension_pnp
							));
			end if;
		end;	
		
		procedure native is
			use et_csv;

			column_item			: constant string := "ITEM";
			column_device		: constant string := "DEVICE";
			column_face			: constant string := "FACE";
			column_x			: constant string := "X";
			column_y			: constant string := "Y";
			column_rotation		: constant string := "ROTATION";
-- 			column_value		: constant string := "VALUE";
-- 			column_package		: constant string := "PACKAGE";
-- 			column_partcode		: constant string := "PARTCODE";
-- 			column_purpose		: constant string := "PURPOSE";

			procedure query_device (cursor : in type_devices.cursor) is
				use type_devices;
				use et_pcb_coordinates.pac_geometry_brd;
			begin
				put_field (file => pnp_handle); -- CS item number
				put_field (file => pnp_handle, text => to_string (key (cursor))); -- R4
				put_field (file => pnp_handle, text => to_string (get_face (element (cursor).position))); -- top/bottom
				put_field (file => pnp_handle, text => to_string (x (element (cursor).position))); -- X
				put_field (file => pnp_handle, text => to_string (y (element (cursor).position))); -- Y
				put_field (file => pnp_handle, text => to_string (rot (element (cursor).position))); -- rotation
-- 				put_field (file => pnp_handle, text => to_string (element (cursor).value)); -- 100R
-- 				put_field (file => pnp_handle, text => to_string (element (cursor).packge)); -- S_0805.pac
-- 				put_field (file => pnp_handle, text => to_string (element (cursor).partcode)); -- R_PAC_S_0805_VAL_100R
-- 				put_field (file => pnp_handle, text => to_string (element (cursor).purpose)); -- purpose

				put_lf (file => pnp_handle, field_count => et_csv.column);
			end query_device;
			
		begin -- native
			-- CS: A nice header should be placed. First make sure stock_manager can handle it.
			
			-- write the table header
			et_csv.reset_column;
			put_field (file => pnp_handle, text => column_item);
			put_field (file => pnp_handle, text => column_device);
			put_field (file => pnp_handle, text => column_face);
			put_field (file => pnp_handle, text => column_x);
			put_field (file => pnp_handle, text => column_y);
			put_field (file => pnp_handle, text => column_rotation);
			put_lf    (file => pnp_handle, field_count => et_csv.column);

			type_devices.iterate (
				container	=> pnp,
				process		=> query_device'access);
			
			-- CS: A list end mark should be placed.
			-- put_line (pnp_handle, comment_mark & " end of list");
			
		end;
		
	begin -- write_pnp
		-- build the file name
		set_file_name;

		log (text => "writing pick & place file " & enclose_in_quotes (to_string (file_name)) & " ...", level => log_threshold);
		
		create (
			file => pnp_handle,
			mode => out_file, 
			name => to_string (file_name));

		case format is
			when NATIVE => native;
-- 			when EAGLE => eagle;
			when others => raise constraint_error;
		end case;
				
		close (pnp_handle);

		exception
			when event: others =>
				if is_open (pnp_handle) then
					close (pnp_handle);
				end if;
				
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
				
	end write_pnp;

		
end et_pick_and_place;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
