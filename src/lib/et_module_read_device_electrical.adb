------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / ELECTRICAL DEVICE                       --
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
-- - clean up
-- - rename global variables
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;
with ada.directories;

with et_module_names;				use et_module_names;
with et_schematic_geometry;
with et_schematic_coordinates;

with et_keywords;					use et_keywords;
with et_pcb_sides;
with et_board_geometry;
with et_board_coordinates;

with et_coordinates_formatting;		use et_coordinates_formatting;
with et_device_name;				use et_device_name;

with et_device_model;
with et_device_appearance;
with et_device_purpose;
with et_device_model_names;
with et_device_value;
with et_device_library;				use et_device_library;
with et_device_partcode;
with et_device_sections;
with et_package_variant;
with et_symbol_read;
with et_schematic_text;
with et_device_read;
with et_devices_electrical;
with et_devices_non_electrical;
with et_pcb_stack;

with et_package_read;

with et_package_name;
with et_package_model_name;

with et_conventions;

with et_schematic_ops;
with et_schematic_ops.units;
with et_board_ops;

with et_device_placeholders;
with et_device_placeholders.packages;
with et_device_placeholders.symbols;

with et_board_outline;
with et_pcb_placeholders;
with et_unit_name;
with et_units;
with et_mirroring;						use et_mirroring;
with et_alignment;						use et_alignment;
with et_object_status;

with et_general_rw;						use et_general_rw;



package body et_module_read_device_electrical is

	use pac_generic_modules;

	

	-- The temporarily device will exist where "device" points at:
	device					: access et_devices_electrical.type_device_electrical;
	
	device_name				: et_device_name.type_device_name; -- C12
	device_model			: et_device_model_names.pac_device_model_file.bounded_string; -- ../libraries/transistor/pnp.dev
	
	device_value			: et_device_value.pac_device_value.bounded_string; -- 470R
	device_appearance		: et_units.type_appearance_schematic;

	
	device_partcode	: et_device_partcode.pac_device_partcode.bounded_string;
	device_purpose	: et_device_purpose.pac_device_purpose.bounded_string;
	device_variant	: et_package_variant.pac_package_variant_name.bounded_string; -- D, N

	
	-- temporarily collection of units:
	device_units	: et_units.pac_units.map; -- PWR, A, B, ...

	device_position	: et_board_coordinates.type_package_position; -- in the layout ! incl. angle and face
	

		
	procedure read_device (
		line : in type_fields_of_line)
	is
		use et_device_model;
		use et_device_purpose;
		use et_device_model_names;
		use et_devices_electrical;
		use et_device_appearance;
		use et_device_value;
		use et_device_partcode;
		use et_package_variant;
		
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name C12
			expect_field_count (line, 2);
			device_name := to_device_name (f (line, 2));

		-- As soon as the appearance becomes clear, a temporarily device is
		-- created where pointer "device" is pointing at:
		elsif kw = keyword_appearance then -- sch_pcb, sch
			expect_field_count (line, 2);
			device_appearance := to_appearance (f (line, 2));

			case device_appearance is
				when APPEARANCE_VIRTUAL =>
					device := new type_device_electrical'(
						appearance	=> APPEARANCE_VIRTUAL,
						others		=> <>);

				when APPEARANCE_PCB =>
					device := new type_device_electrical'(
						appearance	=> APPEARANCE_PCB,
						others		=> <>);
			end case;
					
		elsif kw = keyword_value then -- value 100n
			expect_field_count (line, 2);

			-- validate value
			device_value := to_value_with_check (f (line, 2));

		elsif kw = keyword_model then -- model /models/capacitor.dev
			expect_field_count (line, 2);
			device_model := to_file_name (f (line, 2));
			
		elsif kw = keyword_variant then -- variant S_0805, N, D
			expect_field_count (line, 2);
			check_variant_name_length (f (line, 2));
			device_variant := to_variant_name (f (line, 2));

		elsif kw = keyword_partcode then -- partcode LED_PAC_S_0805_VAL_red
			expect_field_count (line, 2);

			-- validate partcode
			device_partcode := to_partcode (f (line, 2));

		elsif kw = keyword_purpose then -- purpose power_out
			expect_field_count (line, 2);

			-- validate purpose
			device_purpose := to_purpose (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end read_device;



		
		
		

				
						
	



	procedure insert_device (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 
	
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_devices_electrical;
			use et_device_model;
			use et_device_model_names;
			use et_package_name;
			use et_package_model_name;
			use et_pcb_stack;
			use et_package_variant;
			use pac_package_variant_name;
			
			device_cursor : pac_devices_electrical.cursor;
			inserted : boolean;

			
			-- Derives package name from device.model and device.variant.
			-- Checks if variant exits in device.model.
			function get_package_name return pac_package_name.bounded_string is
				name : pac_package_name.bounded_string; -- S_SO14 -- to be returned
				device_cursor : pac_device_models.cursor;

				
				procedure query_variants (
					model	: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
					dev_lib	: in type_device_model) -- a device in the library 
				is
					use pac_package_variants;
					variant_cursor : pac_package_variants.cursor;
					use ada.directories;
					
				begin -- query_variants
					-- Locate the variant (specified by the device in the module) in
					-- the device model.
					variant_cursor := pac_package_variants.find (
						container	=> dev_lib.variants,
						key			=> device.variant); -- the variant name from the module !

					-- The variant should be there. Otherwise abort.
					if variant_cursor = pac_package_variants.no_element then
						log (ERROR, "variant " & to_string (device.variant) &
							" not available in device model " & to_string (model) & " !", console => true);
						raise constraint_error;
					else
						name := to_package_name (base_name (to_string (element (variant_cursor).package_model)));
					end if;
				end;

				
			begin -- get_package_name
				log_indentation_up;
				log (text => "verifying package variant " & to_string (device.variant) &
						" in device model " & to_string (device.model) & " ... ", level => log_threshold + 2);

				-- Locate the device in the library. CS: It should be there, otherwise exception arises here:
				device_cursor := pac_device_models.find (
					container	=> et_device_library.device_library,
					key			=> device.model); -- libraries/devices/7400.dev

				-- Query package variants
				pac_device_models.query_element (
					position	=> device_cursor,
					process		=> query_variants'access);
				
				log_indentation_down;
				return name;
			end get_package_name;


			use et_board_ops;
			use et_device_read;
			use et_device_appearance;
			use et_device_purpose;
			use et_device_value;				
			use et_device_partcode;

			
		begin
			log (text => "device " & to_string (device_name), level => log_threshold + 1);
			log_indentation_up;

			if not et_conventions.prefix_valid (device_name) then 
				--log (message_warning & "prefix of device " & et_libraries.to_string (device_name) 
				--	 & " not conformant with conventions !");
				null; -- CS output something helpful
			end if;
			
			-- assign temporarily variable for model:
			device.model := device_model;

			-- assign appearance specific temporarily variables and write log information
			if device.appearance = APPEARANCE_PCB then

				if not value_characters_valid (device_value) then
					log (WARNING, "value of " & to_string (device_name) &
							" contains invalid characters !");
					log_indentation_reset;
					value_invalid (to_string (device_value));
				end if;
				
				log (text => "value " & to_string (device_value), level => log_threshold + 2);
				device.value := device_value;
				if not et_conventions.value_valid (device_value, get_prefix (device_name)) then
					log (WARNING, "value of " & to_string (device_name) &
						" not conformant with conventions !");
				end if;

				log (text => "partcode " & to_string (device_partcode), level => log_threshold + 2);
				if partcode_characters_valid (device_partcode) then
					device.partcode	:= device_partcode;
				else
					log_indentation_reset;
					partcode_invalid (to_string (device_partcode));
				end if;

				log (text => "purpose " & to_string (device_purpose), level => log_threshold + 2);
				if purpose_characters_valid (device_purpose) then
					device.purpose	:= device_purpose;
				else
					log_indentation_reset;
					purpose_invalid (to_string (device_purpose));
				end if;

				log (text => "variant " & to_string (device_variant), level => log_threshold + 2);
				check_variant_name_characters (device_variant);
				device.variant	:= device_variant;

				-- CS: warn operator if provided but ignored due to the fact that device is virtual
			end if;

			pac_devices_electrical.insert (
				container	=> module.devices,
				position	=> device_cursor,
				inserted	=> inserted,
				key			=> device_name, -- IC23, R5, LED12
				new_item	=> device.all);

			-- The device name must not be in use by any electrical device:
			if not inserted then
				et_devices_electrical.device_name_in_use (device_name);
			end if;

			-- The device name must not be in use by any non-electrical device:
			if module.devices_non_electric.contains (device_name) then
				et_devices_non_electrical.device_name_in_use (device_name);
			end if;

			
			-- Read the device model (like ../libraries/transistor/pnp.dev) and
			-- check the conductor layers:
			read_device (
				file_name		=> device.model,
				check_layers	=> (check => YES, deepest_layer => get_deepest_conductor_layer (module_cursor)),
				log_threshold	=> log_threshold + 2);

			-- Validate partcode according to category, package and value:
			if device.appearance = APPEARANCE_PCB then
				et_conventions.validate_partcode (
					partcode		=> device.partcode,
					device_name		=> device_name,

					-- Derive package name from device.model and device.variant.
					-- Check if variant specified in device.model.
					packge			=> get_package_name, 
					
					value			=> device.value,
					log_threshold	=> log_threshold + 2);
			end if;
			
			-- reset pointer "device" so that the old device gets destroyed
			device := null;
			-- CS free memory ?

			-- clean up temporarily variables for next device
			-- CS ? device_name		:= (others => <>);
			device_model	:= to_file_name ("");
			device_value	:= pac_device_value.to_bounded_string ("");
			device_purpose	:= pac_device_purpose.to_bounded_string ("");
			device_partcode := pac_device_partcode.to_bounded_string ("");
			device_variant	:= to_variant_name ("");

			log_indentation_down;
		end query_module;
	
	
	
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert device", level => log_threshold);
			
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
	
		
		log_indentation_down;
	
	end insert_device;
	
	
	



	
	
	
	
	procedure read_package_position (
		line : in type_fields_of_line)
	is
		use et_board_coordinates;
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
	

	
	
	
	
	procedure set_package_position is begin
		-- Assign coordinates of package to temporarily device:
		-- CS: constraint error will arise here if the device is virtual.
		-- issue warning and skip this statement in this case:
		device.position := device_position;

		-- reset device package position for next device
		device_position := et_board_coordinates.package_position_default;
	end set_package_position;
	
	
	
	
	

	device_unit_mirror		: type_mirror := MIRROR_NO;
	device_unit_name		: et_unit_name.pac_unit_name.bounded_string; -- GPIO_BANK_1
	device_unit_position	: et_schematic_coordinates.type_object_position; -- x,y,sheet,rotation

	unit_placeholder_reference	: et_device_placeholders.symbols.type_text_placeholder (meaning => et_device_placeholders.NAME);
	unit_placeholder_value		: et_device_placeholders.symbols.type_text_placeholder (meaning => et_device_placeholders.VALUE);
	unit_placeholder_purpose	: et_device_placeholders.symbols.type_text_placeholder (meaning => et_device_placeholders.PURPOSE);

	
	
	
	
	procedure read_unit (
		line : in type_fields_of_line)
	is
		use et_schematic_geometry;	
		use pac_geometry_2;
		use et_schematic_coordinates;
		use et_units;
		use et_unit_name;
		use pac_unit_name;
		
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_name then -- name 1, GPIO_BANK_1, ...
			expect_field_count (line, 2);
			device_unit_name := to_unit_name (f (line, 2));
			
		elsif kw = keyword_position then -- position sheet 1 x 1.000 y 5.555 rotation 180.0
			expect_field_count (line, 9);

			-- extract position of unit starting at field 2
			device_unit_position := to_position (line, 2);


		elsif kw = keyword_mirrored then -- mirrored no/x_axis/y_axis
			expect_field_count (line, 2);
			device_unit_mirror := to_mirror_style (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_unit;

	
	
	
	
	
	
	
	procedure insert_unit (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 


		procedure insert_unit is 
			use et_schematic_coordinates;
			use et_units;
			use et_unit_name;
			use et_device_appearance;
			use et_object_status;
		begin
			log_indentation_up;
			-- log (text => "unit " & to_string (device_unit_name), log_threshold + 1);
			-- No good idea. Confuses operator because units are collected BEFORE the device is complete.
			
			-- Depending on the appearance of the device, a virtual or real unit
			-- is inserted in the unit list of the device.
			
			case device_appearance is
				when APPEARANCE_VIRTUAL =>
					pac_units.insert (
						container	=> device_units,
						key			=> device_unit_name,
						new_item	=> (
							appearance	=> APPEARANCE_VIRTUAL,
							status		=> get_default_status,
							mirror		=> device_unit_mirror,
							position	=> device_unit_position));
											
				when APPEARANCE_PCB =>
					-- A unit of a real device has placeholders:
					pac_units.insert (
						container	=> device_units,
						key			=> device_unit_name,
						new_item	=> (
							mirror		=> device_unit_mirror,
							status		=> get_default_status,
							position	=> device_unit_position,
							appearance	=> APPEARANCE_PCB,
							placeholders => (
								-- The placeholders for reference, value and purpose have
								-- been built and can now be assigned to the unit:
								name		=> unit_placeholder_reference,
								value 		=> unit_placeholder_value,
								purpose		=> unit_placeholder_purpose)));
			end case;

			-- clean up for next unit
			device_unit_position := zero_position;
			device_unit_name := unit_name_default;
			--device_unit := (others => <>);
			device_unit_mirror := MIRROR_NO;
			--device_unit_rotation := geometry.zero_rotation;

			-- CS reset placeholders for name, value and purpose ?

			log_indentation_down;
		end insert_unit;


		
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " insert unit", level => log_threshold);
			
		log_indentation_up;
		
		-- CS
		-- update_element (
		-- 	container	=> generic_modules,
		-- 	position	=> module_cursor,
		-- 	process		=> query_module'access);
		insert_unit;
		
		log_indentation_down;
	
	end insert_unit;

	
	
	
	
	
	procedure insert_units is begin
			-- insert temporarily collection of units in device
		device.units := device_units;

		-- clear temporarily collection of units for next device
		et_units.pac_units.clear (device_units);
	end insert_units;
	
	
	
	
	
	-- temporarily placeholders of unit name (IC12), value (7400) and purpose (clock buffer)
	unit_placeholder			: et_schematic_text.type_text_basic;
	unit_placeholder_position	: et_schematic_geometry.pac_geometry_2.type_vector_model;
	unit_placeholder_meaning	: et_device_placeholders.type_placeholder_meaning := et_device_placeholders.placeholder_meaning_default;

	
	
	
	procedure read_unit_placeholder (
		line : in type_fields_of_line)
	is
		use et_device_placeholders;
		use et_schematic_text;
		use et_symbol_read;
		use et_schematic_geometry.pac_geometry_2;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_meaning then -- meaning reference, value or purpose
			expect_field_count (line, 2);
			unit_placeholder_meaning := to_meaning (f (line, 2));
			
		elsif kw = keyword_position then -- position x 0.000 y 5.555
			expect_field_count (line, 5);

			-- extract position of placeholder starting at field 2
			unit_placeholder_position := to_vector_model (line, 2);

		elsif kw = keyword_size then -- size 3.0
			expect_field_count (line, 2);
			unit_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_rotation then -- rotation 90.0
			expect_field_count (line, 2);

			unit_placeholder.rotation := pac_text_schematic.to_rotation_doc (f (line, 2));

-- 											elsif kw = keyword_style then -- stlye italic
-- 												expect_field_count (line, 2);
-- 
-- 												unit_placeholder.style := et_symbol_model.to_text_style (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment of placeholder starting at field 2
			unit_placeholder.alignment := to_alignment (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end read_unit_placeholder;

	
	
	
	
	
	procedure build_unit_placeholder is
		use et_device_placeholders;
		use et_schematic_coordinates;	
		use et_schematic_geometry;
	begin
		case unit_placeholder_meaning is
			when NAME =>
				unit_placeholder_reference := (unit_placeholder with
					meaning		=> NAME,
					position	=> unit_placeholder_position);
				
			when VALUE =>
				unit_placeholder_value := (unit_placeholder with
					meaning		=> VALUE,
					position	=> unit_placeholder_position);

			when PURPOSE =>
				unit_placeholder_purpose := (unit_placeholder with
					meaning		=> PURPOSE,
					position	=> unit_placeholder_position);

			when others =>
				log (ERROR, "meaning of placeholder not supported !", console => true);
				raise constraint_error;
		end case;

		-- clean up for next placeholder
		unit_placeholder := (others => <>);
		unit_placeholder_meaning := placeholder_meaning_default;
		unit_placeholder_position := pac_geometry_2.origin;
		
	end build_unit_placeholder;


	
	

	
	
	
	-- These two variables assist when a particular placeholder is appended to the
	-- list of placholders in silk screen, assy doc and their top or bottom face:
	device_text_placeholder_position: et_board_coordinates.type_package_position := et_board_coordinates.placeholder_position_default; -- incl. rotation and face

	
	device_text_placeholder_layer : et_device_placeholders.packages.type_placeholder_layer := 
	et_device_placeholders.packages.type_placeholder_layer'first; -- silkscreen/assembly_documentation

	
	
	-- a single temporarily placeholder of a package
	device_text_placeholder : et_device_placeholders.packages.type_text_placeholder;

	
	-- the temporarily collection of placeholders of packages (in the layout)
	device_text_placeholders	: et_device_placeholders.packages.type_text_placeholders; -- silk screen, assy doc, top, bottom


	
	
	procedure read_device_text_placeholder (
		line : in type_fields_of_line)
	is
		use et_device_placeholders;
		use et_device_placeholders.packages;
		use et_board_geometry;
		use et_board_geometry.pac_geometry_2;
		use et_board_coordinates;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_meaning then -- meaning name, value, ...
			expect_field_count (line, 2);
			device_text_placeholder.meaning := to_meaning (f (line, 2));
			
		elsif kw = keyword_layer then -- layer silkscreen/assy_doc
			expect_field_count (line, 2);
			device_text_placeholder_layer := to_placeholder_layer (f (line, 2));

		elsif kw = keyword_anchor then -- anchor relative/absolute
			expect_field_count (line, 2);
			device_text_placeholder.anchor_mode := to_anchor_mode (f (line, 2));
			
		elsif kw = keyword_position then -- position x 0.000 y 5.555 rotation 0.00 face top
			expect_field_count (line, 9);

			-- extract position of placeholder starting at field 2
			device_text_placeholder_position := to_position (line, 2);

		elsif kw = keyword_size then -- size 5
			expect_field_count (line, 2);
			device_text_placeholder.size := to_distance (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.15
			expect_field_count (line, 2);

			device_text_placeholder.line_width := to_distance (f (line, 2));

		elsif kw = keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment of placeholder starting at field 2
			device_text_placeholder.alignment := to_alignment (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end read_device_text_placeholder;

		
		
	
	
	
	procedure insert_package_placeholder is
		use et_device_placeholders.packages;
		use et_pcb_sides;
		use et_board_coordinates;
	begin
		device_text_placeholder.position := et_board_geometry.pac_geometry_2.type_position (device_text_placeholder_position);
		
		case device_text_placeholder_layer is
			when SILKSCREEN => 
				case get_face (device_text_placeholder_position) is

					when TOP =>
						pac_text_placeholders.append (
							container	=> device_text_placeholders.silkscreen.top,
							new_item	=> device_text_placeholder);
						
					when BOTTOM =>
						pac_text_placeholders.append (
							container	=> device_text_placeholders.silkscreen.bottom,
							new_item	=> device_text_placeholder);
				end case;
				
			when ASSY_DOC =>
				case get_face (device_text_placeholder_position) is

					when TOP =>
						pac_text_placeholders.append (
							container	=> device_text_placeholders.assy_doc.top,
							new_item	=> device_text_placeholder);

					when BOTTOM =>
						pac_text_placeholders.append (
							container	=> device_text_placeholders.assy_doc.bottom,
							new_item	=> device_text_placeholder);
				end case;

		end case;

		-- reset placeholder for next placeholder
		device_text_placeholder := (others => <>);
		device_text_placeholder_position := placeholder_position_default;

	end insert_package_placeholder;

	
	
	
	
	procedure insert_placeholders is begin
		-- Insert placeholder collection in temporarily device:
		-- CS: constraint error will arise here if the device is virtual.
		-- issue warning and skip this statement in this case:
		device.placeholders := device_text_placeholders;

		-- clean up for next collection of placeholders
		device_text_placeholders := (others => <>);
	end insert_placeholders;
	
	
	
end et_module_read_device_electrical;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
