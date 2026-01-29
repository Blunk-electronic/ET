------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / NON-ELECTRICAL DEVICE                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- ToDo:
--
--
--
--

with ada.text_io;					use ada.text_io;
with ada.directories;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_pcb_sides;
with et_board_geometry;
with et_board_coordinates;			use et_board_coordinates;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_device_name;				use et_device_name;

with et_device_purpose;
with et_device_value;
with et_device_partcode;
with et_package_variant;

with et_devices_electrical;			
with et_devices_non_electrical;		use et_devices_non_electrical;
with et_pcb_stack;

with et_package_library;
with et_package_read;
with et_package_name;
with et_package_model_name;

with et_conventions;

with et_device_placeholders.packages;

with et_module;							use et_module;

with et_mirroring;						use et_mirroring;
with et_alignment;						use et_alignment;




package body et_module_read_device_non_electrical is

	use pac_generic_modules;

	

	-- The temporarily non-electrical device:
	device			: et_devices_non_electrical.type_device_non_electrical;

	device_name		: et_device_name.type_device_name; -- C12
	
	device_value	: et_device_value.pac_device_value.bounded_string; -- 470R

	device_partcode	: et_device_partcode.pac_device_partcode.bounded_string;
	device_purpose	: et_device_purpose.pac_device_purpose.bounded_string;
	device_variant	: et_package_variant.pac_package_variant_name.bounded_string; -- D, N

	device_position	: et_board_coordinates.type_package_position; -- in the layout ! incl. angle and face	
	
	package_model_name	: et_package_model_name.pac_package_model_file.bounded_string; -- ../libraries/misc/fiducials/crosshair.pac

	-- the temporarily collection of placeholders of packages (in the layout)
	text_placeholders	: et_device_placeholders.packages.type_text_placeholders; -- silk screen, assy doc, top, bottom



	
	
		
	procedure read_device_non_electrical (
		line : in type_fields_of_line)
	is
		use et_package_model_name;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name FD1
			expect_field_count (line, 2);
			device_name := to_device_name (f (line, 2));

			
		elsif kw = keyword_position then -- position x 163.500 y 92.500 rotation 0.00 face top
			expect_field_count (line, 9);

			-- extract device position (in the layout) starting at field 2
			device_position := to_position (line, 2);
		
			
		elsif kw = keyword_model then -- model /lib/fiducials/crosshair.pac
			expect_field_count (line, 2);
			package_model_name := to_package_model_name (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_device_non_electrical;



		
						
	


	

	procedure insert_device_non_electrical (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 
	
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is


			procedure read_package_model is 
				use et_pcb_stack;
				use et_package_read;
			begin
				log (text => "read package model", level => log_threshold + 2);
				log_indentation_up;
				
				-- Read the package model (like ../libraries/fiducials/crosshair.pac):
				read_package (
					file_name		=> package_model_name,
					-- CS check_layers	=> YES,
					log_threshold	=> log_threshold + 3);

				log_indentation_down;
			end read_package_model;



			procedure validate_prefix is
				use et_conventions;
			begin
				log (text => "validate prefix", level => log_threshold + 2);
				log_indentation_up;
				
				if not prefix_valid (device_name) then 
					--log (message_warning & "prefix of device " & et_libraries.to_string (device_name) 
					--	 & " not conformant with conventions !");
					null; -- CS output something helpful
				end if;					

				log_indentation_down;
			end validate_prefix;



			inserted : boolean;			
			
			-- Adds the package to the board:
			procedure add_package_to_board is 
				device_cursor : pac_devices_non_electrical.cursor;
			begin
				log (text => "add package to board", level => log_threshold + 2);
				log_indentation_up;

				pac_devices_non_electrical.insert (
					container	=> module.devices_non_electric,
					position	=> device_cursor,
					inserted	=> inserted,
					key			=> device_name, -- FD1, H1
					new_item	=> device);

				log_indentation_down;
			end;



			-- Tests the "inserted" flag and issues a log message.
			-- The inserted-flag indicates that the device does not exist
			-- already:
			procedure check_for_name_in_use is begin
				log (text => "check for name in use", level => log_threshold + 2);
				log_indentation_up;

				-- The device name must not be in use by any non-electrical device:
				if not inserted then
					et_devices_non_electrical.device_name_in_use (device_name);
				end if;

				-- The device name must not be in use by an electrical device:
				if module.devices.contains (device_name) then
					et_devices_electrical.device_name_in_use (device_name);
				end if;
				
				log_indentation_down;
			end check_for_name_in_use;


			
			
			procedure clean_up is begin
				-- clean up for next non-electrical device:
				device 				:= (others => <>);
				device_name			:= (others => <>);
				device_position		:= package_position_default;
				text_placeholders	:= (others => <>);
			end clean_up;

			
			
		begin
			log (text => "device " & to_string (device_name),
				 level => log_threshold + 1);
			
			log_indentation_up;

			-- The package model must be read first because
			-- later a cursor to the package model is required
			-- to create the link between board device  and package model:
			read_package_model;

			validate_prefix;

			-- Assign the cursor to the package model;
			device.model_cursor := 
				et_package_library.get_package_model (package_model_name);
			
			-- Assign the package position:
			device.position := device_position;
			
			add_package_to_board;

			check_for_name_in_use;
			
			clean_up;

			log_indentation_down;
		end query_module;
	
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert device (non-electrical)", level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
			
		log_indentation_down;	
	end insert_device_non_electrical;
	

	
	

	
	
	
	procedure read_package_position (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 163.500 y 92.500 rotation 0.00 face top
			expect_field_count (line, 9);

			-- extract package position starting at field 2
			device_position := to_position (line, 2);

		else
			invalid_keyword (kw);
		end if;
	end read_package_position;
	

	
	
	



	
	
	
	-- These two variables assist when a particular placeholder is appended to the
	-- list of placholders in silkscreen, assy doc and their top or bottom face:
	text_placeholder_position	: type_package_position := placeholder_position_default; -- incl. rotation and face

	
	text_placeholder_layer : et_device_placeholders.packages.type_placeholder_layer := 
		et_device_placeholders.packages.type_placeholder_layer'first; -- silkscreen/assembly_documentation
	
	
	-- a single temporarily placeholder of a package
	text_placeholder : et_device_placeholders.packages.type_text_placeholder;

	


	
	
	procedure read_device_text_placeholder (
		line : in type_fields_of_line)
	is
		use et_device_placeholders;
		use et_device_placeholders.packages;
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_meaning then -- meaning name, value, ...
			expect_field_count (line, 2);
			text_placeholder.meaning := to_meaning (f (line, 2));
			
		elsif kw = keyword_layer then -- layer silkscreen/assy_doc
			expect_field_count (line, 2);
			text_placeholder_layer := to_placeholder_layer (f (line, 2));

		elsif kw = keyword_anchor then -- anchor relative/absolute
			expect_field_count (line, 2);
			text_placeholder.anchor_mode := to_anchor_mode (f (line, 2));
			
		elsif kw = keyword_position then -- position x 0.000 y 5.555 rotation 0.00 face top
			expect_field_count (line, 9);

			-- extract position of placeholder starting at field 2
			text_placeholder_position := to_position (line, 2);

		elsif kw = keyword_size then -- size 5
			expect_field_count (line, 2);
			text_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.15
			expect_field_count (line, 2);

			text_placeholder.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment of placeholder starting at field 2
			text_placeholder.alignment := to_alignment (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end read_device_text_placeholder;

		
		
	
	
	
	procedure insert_package_placeholder is
		use et_device_placeholders.packages;
		use et_pcb_sides;
	begin
		text_placeholder.position := et_board_geometry.pac_geometry_2.type_position (text_placeholder_position);
		
		case text_placeholder_layer is
			when SILKSCREEN => 
				case get_face (text_placeholder_position) is

					when TOP =>
						pac_text_placeholders.append (
							container	=> text_placeholders.silkscreen.top,
							new_item	=> text_placeholder);
						
					when BOTTOM =>
						pac_text_placeholders.append (
							container	=> text_placeholders.silkscreen.bottom,
							new_item	=> text_placeholder);
				end case;
				
			when ASSY_DOC =>
				case get_face (text_placeholder_position) is

					when TOP =>
						pac_text_placeholders.append (
							container	=> text_placeholders.assy_doc.top,
							new_item	=> text_placeholder);

					when BOTTOM =>
						pac_text_placeholders.append (
							container	=> text_placeholders.assy_doc.bottom,
							new_item	=> text_placeholder);
				end case;

		end case;

		-- reset placeholder for next placeholder
		text_placeholder := (others => <>);
		text_placeholder_position := placeholder_position_default;

	end insert_package_placeholder;

	
	
	
	
	procedure insert_placeholders is begin
		-- Insert placeholder collection in temporarily device:
		device.placeholders := text_placeholders;

		-- clean up for next collection of placeholders
		text_placeholders := (others => <>);

	end insert_placeholders;
	
	
	
end et_module_read_device_non_electrical;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
