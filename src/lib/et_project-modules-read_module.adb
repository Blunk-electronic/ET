------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          ET_PROJECT.READ_MODULE                          --
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

with et_frame_rw;
with et_pcb_rw.device_packages;	use et_pcb_rw.device_packages;

separate (et_project.modules)

procedure read_module (
	file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
	log_threshold	: in et_string_processing.type_log_level) 
is
	previous_input : ada.text_io.file_type renames current_input;

	use et_string_processing;
	
	-- Environment variables like $templates could be in file name.
	-- In order to test whether the given module file exists, file name_name must be expanded
	-- so that the environment variables are replaced by the real paths like:
	-- templates/clock_generator.mod or
	-- /home/user/et_templates/pwr_supply.mod.
	file_name_expanded : constant string := expand (file_name);
		
	file_handle : ada.text_io.file_type;
	use pac_generic_modules;
	module_cursor : pac_generic_modules.cursor; -- points to the module being read
	module_inserted : boolean;

	-- The line read from the the module file:
	line : et_string_processing.type_fields_of_line;

	-- This is the section stack of the module. 
	-- Here we track the sections. On entering a section, its name is
	-- pushed onto the stack. When leaving a section the latest section name is popped.
	max_section_depth : constant positive := 11;
	package stack is new et_general_rw.stack_lifo (
		item	=> type_section,
		max 	=> max_section_depth);


	-- META DATA
	meta_basic		: et_meta.type_basic;
	
	meta_schematic	: et_meta.type_schematic;
	prf_libs_sch	: et_meta.pac_preferred_libraries_schematic.list;

	meta_board		: et_meta.type_board;
	prf_libs_brd	: et_meta.pac_preferred_libraries_board.list;

	
	-- RULES
	rules			: et_schematic.type_rules := (others => <>);
-- 	rules_layout	: et_design_rules.pac_file_name.bounded_string;
	-- CS ERC rules ?
	
	active_assembly_variant : et_general.pac_assembly_variant_name.bounded_string; -- "low_cost"
	
	-- Assigns to the module the active assembly variant.
	procedure set_active_assembly_variant is
		use et_schematic;
		use et_assembly_variants;
		
		kw : constant string := f (line, 1);

		procedure set_variant (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			module.active_variant := active_assembly_variant;
		end;
		
	begin
		if kw = keyword_active then
			expect_field_count (line, 2);
			active_assembly_variant := to_variant (f (line, 2));
		else
			invalid_keyword (kw);
		end if;

		update_element (generic_modules, module_cursor, set_variant'access);
	end set_active_assembly_variant;

	-- Assigns the collected meta data to the module:
	procedure set_meta is
		use et_schematic;
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_meta.pac_preferred_libraries_schematic;
		begin
			-- CS check whether date drawn <= date checked <= date_approved
			--  use type_basic for the test of schematic and board data.
			
			module.meta.schematic := meta_schematic;
			module.meta.board := meta_board;
		end;
	begin -- set_meta
		log (text => "meta data ...", level => log_threshold + 1);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);
	end set_meta;

	-- Reads basic meta data. If given line does not contain
	-- basic meta stuff, returns a false.
	function read_meta_basic return boolean is
		use et_meta;
		kw : constant string := f (line, 1);
		result : boolean := true;
	begin
		if kw = keyword_company then
			expect_field_count (line, 2);
			meta_basic.company := to_company (f (line, 2));

		elsif kw = keyword_customer then
			expect_field_count (line, 2);
			meta_basic.customer := to_customer (f (line, 2));
			
		elsif kw = keyword_partcode then
			expect_field_count (line, 2);
			meta_basic.partcode := to_partcode (f (line, 2));
			
		elsif kw = keyword_drawing_number then
			expect_field_count (line, 2);
			meta_basic.drawing_number := to_drawing_number (f (line, 2));
			
		elsif kw = keyword_revision then
			expect_field_count (line, 2);
			meta_basic.revision := to_revision (f (line, 2));
			
		elsif kw = keyword_drawn_by then
			expect_field_count (line, 2);
			meta_basic.drawn_by := to_person (f (line, 2));
			
		elsif kw = keyword_drawn_date then
			expect_field_count (line, 2);
			meta_basic.drawn_date := to_date (f (line, 2));
			
		elsif kw = keyword_checked_by then
			expect_field_count (line, 2);
			meta_basic.checked_by := to_person (f (line, 2));
			
		elsif kw = keyword_checked_date then
			expect_field_count (line, 2);
			meta_basic.checked_date := to_date (f (line, 2));
			
		elsif kw = keyword_approved_by then
			expect_field_count (line, 2);
			meta_basic.approved_by := to_person (f (line, 2));
			
		elsif kw = keyword_approved_date then
			expect_field_count (line, 2);
			meta_basic.approved_date := to_date (f (line, 2));

		else
			result := false;
		end if;
		
		return result;
	end read_meta_basic;
		
	procedure read_meta_schematic is 
		use et_meta;
		kw : constant string := f (line, 1);
	begin
		-- first parse line for basic meta stuff.
		-- if no meta stuff found, test for schematic specific meta data:
		if read_meta_basic = false then
			-- CS: in the future, if there is schematic specific meta data:
			-- if kw = keyword_xyz then
			-- do something
			--else
			invalid_keyword (kw);
		end if;
	end;

	procedure read_meta_board is 
		use et_meta;			
		kw : constant string := f (line, 1);
	begin
		-- first parse line for basic meta stuff.
		-- if no meta stuff found, test for bord specific meta data:
		if read_meta_basic = false then
			-- CS: in the future, if there is schematic specific meta data:
			-- if kw = keyword_xyz then
			-- do something
			--else
			invalid_keyword (kw);
		end if;
	end;		

	procedure read_preferred_lib_schematic is
		kw : constant string := f (line, 1);
		use et_meta;
		lib : pac_preferred_library_schematic.bounded_string;
	begin
		if kw = keyword_path then
			expect_field_count (line, 2);

			lib := to_preferred_library_schematic (f (line, 2));
			
			if not exists (lib) then
				log (WARNING, "Preferred library path for devices " 
					 & enclose_in_quotes (to_string (lib))
					 & " does not exist !");
			end if;

			-- Collect the library path in temporarily list:
			prf_libs_sch.append (lib);
		else
			invalid_keyword (kw);
		end if;
	end read_preferred_lib_schematic;

	procedure read_preferred_lib_board is
		kw : constant string := f (line, 1);
		use et_meta;
		lib : pac_preferred_library_board.bounded_string;
	begin
		if kw = keyword_path then
			expect_field_count (line, 2);
			lib := to_preferred_library_board (f (line, 2));

			if not exists (lib) then
				log (WARNING, "Preferred library path for non-electrical packages " 
					 & enclose_in_quotes (to_string (lib))
					 & " does not exist !");
			end if;
			
			-- Collect the library path in temporarily list:
			prf_libs_brd.append (lib);
		else
			invalid_keyword (kw);
		end if;
	end read_preferred_lib_board;

	
	-- The design rules is simply the name of the DRU file
	-- like JLP_ML4_standard.dru. The content of the DRU file itself
	-- will later be stored in project wide container et_design_rules.design_rules.
	procedure read_rules is
		use et_design_rules;
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_layout then -- layout JLP_ML4_standard.dru
			rules.layout := to_file_name (f (line, 2));
		end if;
	end read_rules;

	-- Assigns the temporarily rules to the module:
	procedure set_rules is
		use et_schematic;
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			use et_design_rules;
		begin
			-- assign rules
			module.rules := rules;

			-- log and read layout design rules if specified. otherwise skip:
			if not is_empty (rules.layout) then
				log (text => keyword_layout & space & to_string (module.rules.layout),
					level => log_threshold + 2);

				-- Read the DRU file like JLP_ML4_standard.dru and store it
				-- in project wide container et_design_rules.design_rules.
				read_rules (rules.layout, log_threshold + 3);
			else
				log (WARNING, "No layout design rules specified ! Defaults will be applied !");
			end if;
				
			-- CS module.rules.erc ?
		end;
		
	begin -- set_rules
		log (text => "design rules ...", level => log_threshold + 1);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		log_indentation_down;
	end set_rules;
	
	function to_position (
		line : in type_fields_of_line; -- "position sheet 3 x 44.5 y 53.5"
		from : in positive)
		return et_coordinates.type_position is
		
		use et_coordinates;
		use pac_geometry_sch;
		
		point : et_coordinates.type_position; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after "sheet" the sheet number
			if f (line, place) = keyword_sheet then
				set_sheet (point, to_sheet (f (line, place + 1)));
				
			-- We expect after the x the corresponding value for x
			elsif f (line, place) = keyword_x then
				--set_x (point, to_distance (f (line, place + 1)));
				set (X, to_distance (f (line, place + 1)), point);

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				--set_y (point, to_distance (f (line, place + 1)));
				set (Y, to_distance (f (line, place + 1)), point);

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	function to_size (
		line : in type_fields_of_line; -- "size x 30 y 40"
		from : in positive)
		return et_submodules.type_submodule_size is
		use et_coordinates.pac_geometry_sch;
		
		size : et_submodules.type_submodule_size; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				size.x := to_distance (f (line, place + 1));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				size.y := to_distance (f (line, place + 1));

			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return size;
	end to_size;

	function to_position (
	-- Returns a type_package_position in the layout.
		line : in type_fields_of_line; -- "position x 23 y 0.2 rotation 90.0 face top"
		from : in positive)
		return et_pcb_coordinates.type_package_position is
		use et_pcb_coordinates;
		use et_pcb_coordinates.pac_geometry_brd;
		
		point : type_package_position; -- to be returned
		place : positive := from; -- the field being read from given line

		-- CS: flags to detect missing sheet, x or y
	begin
		while place <= positive (field_count (line)) loop

			-- We expect after the x the corresponding value for x
			if f (line, place) = keyword_x then
				set (point => point, axis => X, value => to_distance (f (line, place + 1)));

			-- We expect after the y the corresponding value for y
			elsif f (line, place) = keyword_y then
				set (point => point, axis => Y, value => to_distance (f (line, place + 1)));

			-- We expect after "rotation" the corresponding value for the rotation
			elsif f (line, place) = keyword_rotation then
				set (point, to_rotation (f (line, place + 1)));

			-- We expect after "face" the actual face (top/bottom)
			elsif f (line, place) = keyword_face then
				set_face (position => point, face => to_face (f (line, place + 1)));
			else
				invalid_keyword (f (line, place));
			end if;
				
			place := place + 2;
		end loop;
		
		return point;
	end to_position;

	
	-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

	-- drawing grid
	grid_schematic : et_coordinates.pac_geometry_sch.type_grid; -- CS rename to schematic_grid
	grid_board : et_pcb_coordinates.pac_geometry_brd.type_grid; -- CS rename to board_grid

	procedure read_drawing_grid_schematic is 
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_default then -- default x 1.00 y 1.00
			expect_field_count (line, 5);
			grid_schematic := to_grid (line, 2);
		else
			invalid_keyword (kw);
		end if;
	end;

	procedure read_drawing_grid_board is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_default then -- default x 1.00 y 1.00
			expect_field_count (line, 5);
			grid_board := to_grid (line, 2);
		else
			invalid_keyword (kw);
		end if;
	end;

	
	-- net class
	net_class 		: et_pcb.type_net_class;
	net_class_name	: et_pcb.pac_net_class_name.bounded_string;

	procedure reset_net_class is 
		use et_pcb;
	begin
		net_class_name := net_class_name_default;
		net_class := (others => <>);

		-- CS reset parameter-found-flags
	end reset_net_class;

	-- nets
	net_name	: pac_net_name.bounded_string; -- motor_on_off
	net			: et_schematic.type_net;

	strands : et_schematic.pac_strands.list;
	strand	: et_schematic.type_strand;
	net_segments : et_schematic.pac_net_segments.list;
	net_segment	: et_schematic.type_net_segment;
	
	net_labels				: et_schematic.pac_net_labels.list;
	net_label 				: et_schematic.type_net_label_base;
	net_label_rotation		: et_coordinates.type_rotation := pac_geometry_sch.zero_rotation;
	net_label_appearance	: et_schematic.type_net_label_appearance := et_schematic.type_net_label_appearance'first;

	-- The net label direction is relevant if appearance is TAG:
	net_label_direction : et_schematic.type_net_label_direction := et_schematic.type_net_label_direction'first;
	
	net_device_port : et_schematic.type_device_port;
	net_device_ports : et_schematic.pac_device_ports.set;

	net_submodule_port : et_schematic.type_submodule_port;
	net_submodule_ports : et_schematic.pac_submodule_ports.set;

	net_netchanger_port : et_netlists.type_port_netchanger;
	net_netchanger_ports : et_netlists.pac_netchanger_ports.set;
	
	route		: et_pcb.type_route;
	route_via	: et_pcb.type_via;

	sheet_descriptions			: et_frames.pac_schematic_descriptions.map;
	sheet_description_category	: et_frames.type_schematic_sheet_category := 
		et_frames.schematic_sheet_category_default; -- product/develpment/routing
	
	sheet_description_number	: type_sheet := type_sheet'first;				-- 1, 2. 3, ...
	sheet_description_text		: et_text.type_text_content.bounded_string;		-- "voltage regulator"

	-- CS frame_count_schematic		: et_coordinates.type_submodule_sheet_number := et_coordinates.type_submodule_sheet_number'first; -- 10 frames
	frame_template_schematic	: et_frames.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frs
	frame_template_board		: et_frames.pac_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frb
	frame_board_origin : et_pcb_coordinates.pac_geometry_brd.type_point := et_pcb.origin_default; -- x 40 y 60

	procedure read_frame_template_schematic is
	-- Reads the name of the schematic frame template.
		use et_frames;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_1.frs
			expect_field_count (line, 2);
			frame_template_schematic := to_template_name (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end;

	procedure read_frame_template_board is
	-- Reads the name of the board frame template.		
		use et_frames;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_template then -- template $ET_FRAMES/drawing_frame_version_2.frb
			expect_field_count (line, 2);
			frame_template_board := to_template_name (f (line, 2));

		elsif kw = keyword_origin then -- origin x 40 y 60
			expect_field_count (line, 5);
			frame_board_origin := et_pcb_rw.to_position (line, 2);
		else
			invalid_keyword (kw);
		end if;
	end;

	-- Reads the description of a schematic sheet:
	procedure read_sheet_description is
		use et_frames;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_sheet_number then -- number 2
			expect_field_count (line, 2);
			sheet_description_number := to_sheet (f (line, 2));

		elsif kw = keyword_sheet_category then -- category develompent/product/routing
			expect_field_count (line, 2);
			sheet_description_category := to_category (f (line, 2));

		elsif kw = keyword_sheet_description then -- text "voltage regulator"
			expect_field_count (line, 2);
			sheet_description_text := to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_sheet_description;
	
	-- submodules
	submodule_port			: et_submodules.type_submodule_port;
	submodule_port_name		: et_general.pac_net_name.bounded_string; -- RESET
	submodule_ports			: et_submodules.pac_submodule_ports.map;
	submodule_name 			: et_general.pac_module_instance_name.bounded_string; -- MOT_DRV_3
	submodule				: et_submodules.type_submodule;

	note : et_schematic.type_text;

	-- The temporarily device will exist where "device" points at:
	device					: access et_schematic.type_device_sch;
	
	device_name					: type_device_name; -- C12
	device_model				: pac_device_model_file.bounded_string; -- ../libraries/transistor/pnp.dev
	
	
	device_value			: pac_device_value.bounded_string; -- 470R
	device_appearance		: et_schematic.type_appearance_schematic;
	--device_unit				: et_schematic.type_unit;
	--device_unit_rotation	: et_coordinates.type_rotation := geometry.zero_rotation;
	device_unit_mirror		: et_schematic.type_mirror := et_schematic.NO;
	device_unit_name		: pac_unit_name.bounded_string; -- GPIO_BANK_1
	device_unit_position	: et_coordinates.type_position; -- x,y,sheet,rotation


	device_non_electric			: et_schematic.type_device_non_electric;
	device_non_electric_model	: et_packages.pac_package_model_file_name.bounded_string; -- ../libraries/misc/fiducials/crosshair.pac
	
	-- assembly variants
	assembly_variant_name			: et_general.pac_assembly_variant_name.bounded_string; -- low_cost
	assembly_variant_description	: et_assembly_variants.type_description; -- "variant without temp. sensor"
	assembly_variant_devices		: et_assembly_variants.pac_device_variants.map;
	assembly_variant_submodules		: et_assembly_variants.pac_submodule_variants.map;
	
	-- temporarily collection of units:
	device_units	: et_schematic.pac_units.map; -- PWR, A, B, ...
	
	device_partcode	: et_material.type_partcode.bounded_string;
	device_purpose	: pac_device_purpose.bounded_string;
	device_variant	: pac_package_variant_name.bounded_string; -- D, N
	device_position	: et_pcb_coordinates.type_package_position; -- in the layout ! incl. angle and face
	device_flipped	: et_pcb.type_flipped := et_pcb.flipped_default;

	-- These two variables assist when a particular placeholder is appended to the
	-- list of placholders in silk screen, assy doc and their top or bottom face:
	device_text_placeholder_position: et_pcb_coordinates.type_package_position := et_pcb_coordinates.placeholder_position_default; -- incl. rotation and face
	device_text_placeholder_layer	: et_packages.type_placeholder_package_layer := et_packages.type_placeholder_package_layer'first; -- silk_screen/assembly_documentation

	-- a single temporarily placeholder of a package
	device_text_placeholder		: et_packages.type_text_placeholder;

	-- the temporarily collection of placeholders of packages (in the layout)
	device_text_placeholders	: et_packages.type_text_placeholders; -- silk screen, assy doc, top, bottom

	-- temporarily placeholders of unit reference (IC12), value (7400) and purpose (clock buffer)
	unit_placeholder			: et_symbols.type_text_basic;
	unit_placeholder_position	: et_coordinates.pac_geometry_sch.type_point;
	unit_placeholder_meaning	: et_symbols.type_placeholder_meaning := et_symbols.placeholder_meaning_default;
	unit_placeholder_reference	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
	unit_placeholder_value		: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
	unit_placeholder_purpose	: et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE);

	-- temporarily a netchanger is stored here:
	netchanger		: et_submodules.type_netchanger;
	netchanger_id	: et_submodules.type_netchanger_id := et_submodules.type_netchanger_id'first;
				
	-- general board stuff
	board_text : et_packages.type_text_with_content;
	board_text_placeholder : et_pcb.type_text_placeholder;

	
	signal_layers : et_pcb_stack.type_signal_layers.set;
	conductor_layer, dielectric_layer : et_pcb_stack.type_signal_layer := et_pcb_stack.type_signal_layer'first;
	conductor_thickness : et_pcb_stack.type_conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
	dielectric_found : boolean := false;
	board_layer : et_pcb_stack.type_layer;
	board_layers : et_pcb_stack.package_layers.vector;

	-- Whenver a signal layer id is to be read, it must be checked against the
	-- deepest signal layer used. The variable check_layers controls this check.
	-- As preparation we enable the check by setting the "check" to YES.
	-- When section BOARD_LAYER_STACK closes, we also assign the deepest layer used.
	check_layers : et_pcb_stack.type_layer_check (check => et_pcb_stack.YES);

	-- Checks the global signal_layer variable against check_layers:
	procedure validate_signal_layer is 
		use et_pcb_stack;
	begin
		-- Issue warning if signal layer is invalid:
		if not signal_layer_valid (signal_layer, check_layers) then
			signal_layer_invalid (line, signal_layer, check_layers);
		end if;
	end validate_signal_layer;

	-- Checks a given signal layer against check_layers:
	procedure validate_signal_layer (l : in et_pcb_stack.type_signal_layer) is
		use et_pcb_stack;
	begin
		-- Issue warning if signal layer is invalid:
		if not signal_layer_valid (l, check_layers) then
			signal_layer_invalid (line, l, check_layers);
		end if;
	end validate_signal_layer;

	
-- 		board_track_circle : et_pcb.type_copper_circle;
	board_text_copper : et_packages.type_conductor_text;
	board_text_copper_placeholder : et_pcb.type_text_placeholder_conductors;

	net_junctions : et_schematic.type_junctions;
	
	procedure set_junction (place : in string) is begin
		if f (line, 2) = keyword_start then
			net_junctions.start_point := true;
		end if;
		
		if f (line, 2) = keyword_end then
			net_junctions.end_point := true;
		end if;
	end set_junction;

	procedure read_schematic_text is
		use et_coordinates.pac_geometry_sch;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position sheet 2 x 91.44 y 118.56
			expect_field_count (line, 7);

			declare
				-- extract position of note starting at field 2
				pos : constant et_coordinates.type_position := to_position (line, 2);
			begin
				note.position := type_point (pos);
				note.sheet := sheet (pos);
			end;

		elsif kw = keyword_content then -- content "DUMMY TEXT IN CORE MODULE"
			expect_field_count (line, 2); -- actual content in quotes !
			note.content := et_text.to_content (f (line, 2));

		elsif kw = et_text.keyword_size then -- size 1.4
			expect_field_count (line, 2);
			note.size := to_distance (f (line, 2));

		elsif kw = keyword_rotation then -- rotation 90
			expect_field_count (line, 2);
			note.rotation := et_symbols.pac_text.to_rotation_doc (f (line, 2));

-- 			elsif kw = keyword_style then -- style normal/italic
-- 				expect_field_count (line, 2);
			-- note.font := et_symbols.to_text_style (f (line, 2)); -- CS
			-- CS: currently font and style are ignored.

		elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			note.alignment := et_text.to_alignment (line, 2);
			
		else
			invalid_keyword (kw);
		end if;
	end read_schematic_text;

	procedure read_board_text_non_conductor is 
		use et_pcb_coordinates.pac_geometry_brd;
		kw : constant  string := f (line, 1);
	begin
		case stack.parent (degree => 2) is
			when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK 
				| SEC_KEEPOUT | SEC_STENCIL =>

				-- CS: In the following: set a corresponding parameter-found-flag
				if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
					expect_field_count (line, 7);

					-- extract position of note starting at field 2
					board_text.position := to_position (line, 2);

				elsif kw = et_text.keyword_size then -- size 1.000
					expect_field_count (line, 2);
					board_text.size := to_distance (f (line, 2));

				elsif kw = et_text.keyword_line_width then -- line_width 0.1
					expect_field_count (line, 2);
					board_text.line_width := to_distance (f (line, 2));

				elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
					expect_field_count (line, 5);

					-- extract alignment starting at field 2
					board_text.alignment := et_text.to_alignment (line, 2);
					
				elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
					expect_field_count (line, 2); -- actual content in quotes !
					board_text.content := et_text.to_content (f (line, 2));
					
				else
					invalid_keyword (kw);
				end if;
				
			when others => invalid_section;
		end case;
	end read_board_text_non_conductor;

	procedure read_board_text_conductor is
		use et_pcb_coordinates.pac_geometry_brd;
		use et_pcb_stack;
		kw : string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			board_text_copper.position := to_position (line, 2);

		elsif kw = et_text.keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text_copper.size := to_distance (f (line, 2));

		elsif kw = et_text.keyword_line_width then -- line_width 0.1
			expect_field_count (line, 2);
			board_text_copper.line_width := to_distance (f (line, 2));

		elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text_copper.alignment := et_text.to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "TOP", "L2", "BOT"
			expect_field_count (line, 2); -- actual content in quotes !
			board_text_copper.content := et_text.to_content (f (line, 2));

		elsif kw = keyword_layer then -- layer 15
			expect_field_count (line, 2);
			board_text_copper.layer := et_pcb_stack.to_signal_layer (f (line, 2));
			validate_signal_layer (board_text_copper.layer);
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_conductor;
	
	procedure read_board_text_contours is 
		use et_pcb_coordinates.pac_geometry_brd;
		kw : constant  string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
			expect_field_count (line, 7);

			-- extract position of note starting at field 2
			board_text.position := to_position (line, 2);

		elsif kw = et_text.keyword_size then -- size 1.000
			expect_field_count (line, 2);
			board_text.size := to_distance (f (line, 2));

		elsif kw = et_text.keyword_line_width then -- line_width 0.1
			expect_field_count (line, 2);
			board_text.line_width := to_distance (f (line, 2));

		elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
			expect_field_count (line, 5);

			-- extract alignment starting at field 2
			board_text.alignment := et_text.to_alignment (line, 2);
			
		elsif kw = keyword_content then -- content "WATER KETTLE CONTROL"
			expect_field_count (line, 2); -- actual content in quotes !
			board_text.content := et_text.to_content (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_board_text_contours;

	
	procedure read_layer is
		kw : constant string := f (line, 1);
		use et_pcb_stack;
		use package_layers;
		use et_pcb_coordinates.pac_geometry_brd;
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_conductor then -- conductor 1 0.035
			expect_field_count (line, 3);
			conductor_layer := to_signal_layer (f (line, 2));
			conductor_thickness := to_distance (f (line, 3));
			board_layer.conductor.thickness := conductor_thickness;

			-- Layer numbers must be continuous from top to bottom.
			-- After the dielectric of a layer the next conductor layer must
			-- have the next number:
			if dielectric_found then
				if to_index (board_layers.last) /= conductor_layer - 1 then
					log (ERROR, "expect conductor layer number" &
						to_string (to_index (board_layers.last) + 1) & " !",
						console => true);
					raise constraint_error;
				end if;
			end if;
			
			dielectric_found := false;

		elsif kw = keyword_dielectric then -- dielectric 1 1.5
			expect_field_count (line, 3);
			dielectric_layer := to_signal_layer (f (line, 2));
			board_layer.dielectric.thickness := to_distance (f (line, 3));
			dielectric_found := true;
			
			if dielectric_layer = conductor_layer then
				append (board_layers, board_layer);
			else
				log (ERROR, "expect dielectric layer number" & to_string (conductor_layer) & " !", console => true);
				raise constraint_error;
			end if;
		else
			invalid_keyword (kw);
		end if;
	end;

	procedure read_device is
		use et_symbols;
		use et_devices;
		use et_schematic;
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
				when VIRTUAL =>
					device := new type_device_sch'(
						appearance	=> VIRTUAL,
						others		=> <>);

				when PCB =>
					device := new type_device_sch'(
						appearance	=> PCB,
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

		elsif kw = et_material.keyword_partcode then -- partcode LED_PAC_S_0805_VAL_red
			expect_field_count (line, 2);

			-- validate partcode
			device_partcode := et_material.to_partcode (f (line, 2));

		elsif kw = keyword_purpose then -- purpose power_out
			expect_field_count (line, 2);

			-- validate purpose
			device_purpose := to_purpose (f (line, 2));
		else
			invalid_keyword (kw);
		end if;
	end read_device;

	procedure read_device_non_electric is
		use et_packages;
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
			
		elsif kw = keyword_flipped then -- flipped no/yes
			expect_field_count (line, 2);

			device_flipped := et_pcb.to_flipped (f (line, 2));
			
		elsif kw = keyword_model then -- model /lib/fiducials/crosshair.pac
			expect_field_count (line, 2);
			device_non_electric_model := et_packages.to_file_name (f (line, 2));

		else
			invalid_keyword (kw);
		end if;

	end read_device_non_electric;
	
	procedure process_line is 

		procedure execute_section is
		-- Once a section concludes, the temporarily variables are read, evaluated
		-- and finally assembled to actual objects:
			
			procedure insert_net_class (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) 
			is
				use et_pcb;
				inserted : boolean;
				cursor : pac_net_classes.cursor;
			begin -- insert_net_class
				log (text => "net class " & to_string (net_class_name), level => log_threshold + 1);

				-- CS: notify about missing parameters (by reading the parameter-found-flags)
				-- If a parameter is missing, the default is assumed. See type_net_class spec.
				
				pac_net_classes.insert (
					container	=> module.net_classes,
					key			=> net_class_name,
					new_item	=> net_class,
					inserted	=> inserted,
					position	=> cursor);

				if not inserted then
					log (ERROR, "net class '" & et_pcb.to_string (net_class_name) 
							& "' already exists !", console => true);
					raise constraint_error;
				end if;

				reset_net_class; -- clean up for next net class
				
			end insert_net_class;

			procedure add_board_layer (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_pcb_stack;
			begin
				log (text => "board layer stack", level => log_threshold + 1);

				-- Copy the collected layers (except the bottom conductor layer) into the module:
				module.board.stack.layers := board_layers;

				-- If the last entry was "conductor n t" then we assume that this
				-- was the bottom conductor layer (it does not have a dielectric layer underneath).
				if not dielectric_found then
					module.board.stack.bottom.thickness := conductor_thickness;
				else
					log (ERROR, "dielectric not allowed underneath the bottom conductor layer !", console => true);
					raise constraint_error;
				end if;
				
				-- reset layer values:
				dielectric_found := false;
				conductor_layer := et_pcb_stack.type_signal_layer'first;
				dielectric_layer := et_pcb_stack.type_signal_layer'first;
				conductor_thickness := et_pcb_stack.conductor_thickness_outer_default;
				board_layer := (others => <>);
				package_layers.clear (board_layers);

			end add_board_layer;
			
			procedure set_drawing_grid is

				procedure set (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
					use et_coordinates.pac_geometry_sch;
					use et_pcb_coordinates.pac_geometry_brd;
				begin
					module.grid := grid_schematic;
					log (text => "schematic" & to_string (module.grid), level => log_threshold + 2);
					module.board.grid := grid_board;
					log (text => "board" & to_string (module.board.grid), level => log_threshold + 2);
				end;
				
			begin -- set_drawing_grid
				log (text => "drawing grid", level => log_threshold + 1);
				log_indentation_up;
				
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> set'access);

				log_indentation_down;
			end set_drawing_grid;
			
			procedure insert_net (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_schematic;
				inserted : boolean;
				cursor : pac_nets.cursor;
			begin -- insert_net
				log (text => "net " & et_general.to_string (net_name), level => log_threshold + 1);

				-- CS: notify about missing parameters (by reading the parameter-found-flags)
				-- If a parameter is missing, the default is assumed. See type_net spec.
				
				pac_nets.insert (
					container	=> module.nets,
					key			=> net_name,
					new_item	=> net,
					inserted	=> inserted,
					position	=> cursor);

				if not inserted then
					log (ERROR, "net '" & et_general.to_string (net_name) 
						& "' already exists !", console => true);
					raise constraint_error;
				end if;

				-- clean up for next net
				net_name := to_net_name ("");
				net := (others => <>);
				
			end insert_net;
			
			procedure insert_submodule (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_schematic;
				inserted : boolean;
				use et_submodules;
				use et_submodules.pac_submodules;
				cursor : et_submodules.pac_submodules.cursor;
			begin
				log (text => "submodule " & et_general.to_string (submodule_name), level => log_threshold + 1);

				-- CS: notify about missing parameters (by reading the parameter-found-flags)
				-- If a parameter is missing, the default is assumed. See type_submodule spec.
				
				pac_submodules.insert (
					container	=> module.submods,
					key			=> submodule_name,	-- the instance name like MOT_DRV_3
					new_item	=> submodule,
					inserted	=> inserted,
					position	=> cursor);

				if not inserted then
					log (ERROR, "submodule '" & et_general.to_string (submodule_name) 
						& "' already exists !", console => true);
					raise constraint_error;
				end if;

				-- The submodule/template (kept in submodule.file) will be read later once the 
				-- parent module has been read completely.
				
				-- clean up for next submodule
				submodule_name := to_instance_name ("");
				submodule := (others => <>);
				
			end insert_submodule;
			
			procedure set_frame_schematic (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_frames;
			begin
				log (text => "drawing frame schematic " & to_string (frame_template_schematic), level => log_threshold + 1);

				-- set the frame template name
				module.frames.template := frame_template_schematic;

				-- assign the sheet descriptions:
				module.frames.descriptions := sheet_descriptions;

				-- Clean up sheet descriptions even if
				-- there should not be another section for sheet descriptions:
				pac_schematic_descriptions.clear (sheet_descriptions);
				
				-- read the frame template file
				module.frames.frame := et_frame_rw.read_frame (
					file_name		=> frame_template_schematic,
					domain			=> SCHEMATIC,
					log_threshold	=> log_threshold + 2);
				
			end set_frame_schematic;

			procedure add_sheet_description is 
				use et_frames;
				use pac_schematic_descriptions;
				inserted : boolean;
				position : pac_schematic_descriptions.cursor;
			begin
				insert (
					container	=> sheet_descriptions,
					key			=> sheet_description_number,
					inserted	=> inserted,
					position	=> position,
					new_item	=> (sheet_description_text, sheet_description_category)
					);

				-- clean up for next sheet description
				sheet_description_category := schematic_sheet_category_default;
				sheet_description_number := type_sheet'first;
				sheet_description_text := to_content("");
			end add_sheet_description;
			
			procedure set_frame_board (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_frames;
			begin
				log (text => "drawing frame board " & to_string (frame_template_board), level => log_threshold + 1);

				-- set the frame template name
				module.board.frame.template := frame_template_board;

				-- read the frame template file
				module.board.frame.frame := et_frame_rw.read_frame (
					file_name		=> frame_template_board,
					domain			=> PCB,
					log_threshold	=> log_threshold + 2);

				-- set the board origin
				module.board.origin := frame_board_origin;
			end set_frame_board;

			procedure insert_note (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
			begin
				-- append schematic note to collection of notes
				et_schematic.pac_texts.append (module.texts, note);

				-- clean up for next note
				note := (others => <>);
			end insert_note;
			
			procedure insert_package_placeholder is
				use et_packages;
				use et_pcb_coordinates;
			begin
				device_text_placeholder.position := et_pcb_coordinates.pac_geometry_brd.type_position (device_text_placeholder_position);
				
				case device_text_placeholder_layer is
					when SILK_SCREEN => 
						case get_face (device_text_placeholder_position) is

							when TOP =>
								pac_text_placeholders.append (
									container	=> device_text_placeholders.silk_screen.top,
									new_item	=> device_text_placeholder);
								
							when BOTTOM =>
								pac_text_placeholders.append (
									container	=> device_text_placeholders.silk_screen.bottom,
									new_item	=> device_text_placeholder);
						end case;
						
					when ASSEMBLY_DOCUMENTATION =>
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

			procedure insert_unit is 
				use et_symbols;
			begin
				log_indentation_up;
				-- log (text => "unit " & to_string (device_unit_name), log_threshold + 1);
				-- No good idea. Confuses operator because units are collected BEFORE the device is complete.
				
				-- Depending on the appearance of the device, a virtual or real unit
				-- is inserted in the unit list of the device.
				
				case device_appearance is
					when VIRTUAL =>
						et_schematic.pac_units.insert (
							container	=> device_units,
							key			=> device_unit_name,
							new_item	=> (
								appearance	=> VIRTUAL,
								mirror		=> device_unit_mirror,
								position	=> device_unit_position));
												
					when PCB =>
						-- A unit of a real device has placeholders:
						et_schematic.pac_units.insert (
							container	=> device_units,
							key			=> device_unit_name,
							new_item	=> (
								mirror		=> device_unit_mirror,

								position	=> device_unit_position,
								appearance	=> PCB,

								-- The placeholders for reference, value and purpose have
								-- been built and can now be assigned to the unit:
								name		=> unit_placeholder_reference,
								value 		=> unit_placeholder_value,
								purpose		=> unit_placeholder_purpose));
				end case;

				-- clean up for next unit
				device_unit_position := zero_position;
				device_unit_name := et_devices.unit_name_default;
				--device_unit := (others => <>);
				device_unit_mirror := et_schematic.NO;
				--device_unit_rotation := geometry.zero_rotation;

				-- CS reset placeholders for name, value and purpose ?

				log_indentation_down;
			end insert_unit;

			procedure build_unit_placeholder is
			-- Builds a placeholder from unit_placeholder_meaning, unit_placeholder_position and unit_placeholder.
			-- Depending on the meaning of the placeholder it becomes a placeholder 
			-- for the reference (like R4), the value (like 100R) or the purpose (like "brightness control").
				use et_symbols;
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
				unit_placeholder_position := pac_geometry_sch.origin;
				
			end build_unit_placeholder;

			procedure insert_device (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				use et_schematic;
				use et_symbols;
				use et_devices;
				use et_packages;
				use et_pcb_stack;
				
				device_cursor : pac_devices_sch.cursor;
				inserted : boolean;

				function get_package_name return pac_package_name.bounded_string is
				-- Derives package name from device.model and device.variant.
				-- Checks if variant exits in device.model.
					name : pac_package_name.bounded_string; -- S_SO14 -- to be returned
					device_cursor : pac_devices_lib.cursor;

					procedure query_variants (
						model	: in pac_device_model_file.bounded_string; -- libraries/devices/7400.dev
						dev_lib	: in et_devices.type_device_lib) -- a device in the library 
						is
						use pac_variants;
						variant_cursor : pac_variants.cursor;
						use ada.directories;
					begin -- query_variants
						-- Locate the variant (specified by the device in the module) in
						-- the device model.
						variant_cursor := pac_variants.find (
							container	=> dev_lib.variants,
							key			=> device.variant); -- the variant name from the module !

						-- The variant should be there. Otherwise abort.
						if variant_cursor = pac_variants.no_element then
							log (ERROR, "variant " & to_string (device.variant) &
								" not available in device model " & to_string (model) & " !", console => true);
							raise constraint_error;
						else
							name := to_package_name (base_name (et_packages.to_string (element (variant_cursor).package_model)));
						end if;
					end;
					
				begin -- get_package_name
					log_indentation_up;
					log (text => "verifying package variant " & to_string (device.variant) &
							" in device model " & to_string (device.model) & " ... ", level => log_threshold + 2);

					-- Locate the device in the library. CS: It should be there, otherwise exception arises here:
					device_cursor := pac_devices_lib.find (
						container	=> et_devices.devices,
						key			=> device.model); -- libraries/devices/7400.dev

					-- Query package variants
					pac_devices_lib.query_element (
						position	=> device_cursor,
						process		=> query_variants'access);
					
					log_indentation_down;
					return name;
				end get_package_name;
				
			begin -- insert_device
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
				if device.appearance = PCB then

					if not value_characters_valid (device_value) then
						log (WARNING, "value of " & to_string (device_name) &
								" contains invalid characters !");
						log_indentation_reset;
						value_invalid (to_string (device_value));
					end if;
					
					log (text => "value " & to_string (device_value), level => log_threshold + 2);
					device.value := device_value;
					if not et_conventions.value_valid (device_value, prefix (device_name)) then
						log (WARNING, "value of " & to_string (device_name) &
							" not conformant with conventions !");
					end if;

					log (text => "partcode " & et_material.to_string (device_partcode), level => log_threshold + 2);
					if et_material.partcode_characters_valid (device_partcode) then
						device.partcode	:= device_partcode;
					else
						log_indentation_reset;
						et_material.partcode_invalid (et_material.to_string (device_partcode));
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

				pac_devices_sch.insert (
					container	=> module.devices,
					position	=> device_cursor,
					inserted	=> inserted,
					key			=> device_name, -- IC23, R5, LED12
					new_item	=> device.all);

				-- The device name must not be in use by any electrical device:
				if not inserted then
					device_name_in_use (device_name, ELECTRICAL);
				end if;

				-- The device name must not be in use by any non-electrical device:
				if module.devices_non_electric.contains (device_name) then
					device_name_in_use (device_name, NON_ELECTRICAL);
				end if;

				
				-- Read the device model (like ../libraries/transistor/pnp.dev) and
				-- check the conductor layers:
				read_device (
					file_name		=> device.model,
					check_layers	=> (check => YES, deepest_layer => deepest_conductor_layer (module_cursor)),
					log_threshold	=> log_threshold + 2);

				-- Validate partcode according to category, package and value:
				if device.appearance = PCB then
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
				device_partcode := et_material.type_partcode.to_bounded_string ("");
				device_variant	:= to_variant_name ("");

				log_indentation_down;
			end insert_device;						
							
			procedure insert_device_non_electric (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				
				use et_pcb_coordinates;
				use et_pcb;
				use et_schematic;
				
				use et_devices;
				use et_packages;
				use et_pcb_stack;
				
				device_cursor : pac_devices_non_electric.cursor;
				inserted : boolean;

			begin
				log (text => "device (non-electric) " & to_string (device_name), level => log_threshold + 1);
				log_indentation_up;

				if not et_conventions.prefix_valid (device_name) then 
					--log (message_warning & "prefix of device " & et_libraries.to_string (device_name) 
					--	 & " not conformant with conventions !");
					null; -- CS output something helpful
				end if;					

				device_non_electric.position := device_position;
				device_non_electric.flipped := device_flipped;
				device_non_electric.package_model := device_non_electric_model;
-- 					device_non_electric.text_placeholders := device_text_placeholders;

-- 					put_line (count_type'image (et_packages.pac_text_placeholders.length (
-- 						device_non_electric.text_placeholders.silk_screen.top)));

				
				pac_devices_non_electric.insert (
					container	=> module.devices_non_electric,
					position	=> device_cursor,
					inserted	=> inserted,
					key			=> device_name, -- FD1, H1
					new_item	=> device_non_electric);

				-- The device name must not be in use by any non-electrical device:
				if not inserted then
					device_name_in_use (device_name, NON_ELECTRICAL);
				end if;

				-- The device name must not be in use by an electrical device:
				if module.devices.contains (device_name) then
					device_name_in_use (device_name, ELECTRICAL);
				end if;

					
				-- Read the package model (like ../libraries/fiducials/crosshair.pac):
				read_package (
					file_name		=> device_non_electric_model,
-- CS						check_layers	=> YES,
					log_threshold	=> log_threshold + 2);

				-- clean up for next non-electic device:
				device_non_electric 		:= (others => <>);
				device_name					:= (others => <>);
				device_position				:= package_position_default;
				device_flipped				:= flipped_default;
				device_text_placeholders	:= (others => <>);
				device_model				:= to_file_name ("");

				log_indentation_down;
			end insert_device_non_electric;
			

			procedure insert_line (
				layer_cat	: in et_packages.type_layer_category_non_conductor;
				face		: in et_pcb_coordinates.type_face) -- TOP, BOTTOM
			is
			-- The board_line and its board_line_width have been general things until now.
			-- Depending on the layer and the side of the board (face) the board_line
			-- is now assigned to the board where it belongs to.

				use et_packages;
				use et_terminals;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module)
				is
					use et_pcb_coordinates;
				begin
					case face is
						when TOP =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_lines.append (
										container	=> module.board.silk_screen.top.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

								when LAYER_CAT_ASSY =>
									pac_doc_lines.append (
										container	=> module.board.assy_doc.top.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

								when LAYER_CAT_STENCIL =>
									pac_stencil_lines.append (
										container	=> module.board.stencil.top.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
									
								when LAYER_CAT_STOP =>
									pac_stop_lines.append (
										container	=> module.board.stop_mask.top.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_lines.append (
										container	=> module.board.keepout.top.lines,
										new_item	=> (pac_shapes.type_line (board_line) with null record));
									
							end case;
							
						when BOTTOM => null;
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_lines.append (
										container	=> module.board.silk_screen.bottom.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

								when LAYER_CAT_ASSY =>
									pac_doc_lines.append (
										container	=> module.board.assy_doc.bottom.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
									
								when LAYER_CAT_STENCIL =>
									pac_stencil_lines.append (
										container	=> module.board.stencil.bottom.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));
									
								when LAYER_CAT_STOP =>
									pac_stop_lines.append (
										container	=> module.board.stop_mask.bottom.lines,
										new_item	=> (pac_shapes.type_line (board_line) with board_line_width));

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_lines.append (
										container	=> module.board.keepout.bottom.lines,
										new_item	=> (pac_shapes.type_line (board_line) with null record));

							end case;
							
					end case;
				end do_it;
									
			begin -- insert_line
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_line;
				board_reset_line_width;
			end insert_line;

			procedure insert_arc (
				layer_cat	: in et_packages.type_layer_category_non_conductor;
				face		: in et_pcb_coordinates.type_face) -- TOP, BOTTOM
			is
			-- The board_arc and its board_line_width have been general things until now. 
			-- Depending on the layer and the side of the board (face) the board_arc
			-- is now assigned to the board where it belongs to.

				use et_terminals;
				use et_packages;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) 
				is
					use et_pcb_coordinates;
				begin
					case face is
						when TOP =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_arcs.append (
										container	=> module.board.silk_screen.top.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

								when LAYER_CAT_ASSY =>
									pac_doc_arcs.append (
										container	=> module.board.assy_doc.top.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

								when LAYER_CAT_STENCIL =>
									pac_stencil_arcs.append (
										container	=> module.board.stencil.top.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
									
								when LAYER_CAT_STOP =>
									pac_stop_arcs.append (
										container	=> module.board.stop_mask.top.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_arcs.append (
										container	=> module.board.keepout.top.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with null record));
							end case;
							
						when BOTTOM => null;
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_arcs.append (
										container	=> module.board.silk_screen.bottom.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

								when LAYER_CAT_ASSY =>
									pac_doc_arcs.append (
										container	=> module.board.assy_doc.bottom.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
									
								when LAYER_CAT_STENCIL =>
									pac_stencil_arcs.append (
										container	=> module.board.stencil.bottom.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));
									
								when LAYER_CAT_STOP =>
									pac_stop_arcs.append (
										container	=> module.board.stop_mask.bottom.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with board_line_width));

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_arcs.append (
										container	=> module.board.keepout.bottom.arcs,
										new_item	=> (pac_shapes.type_arc (board_arc) with null record));
							end case;
							
					end case;
				end do_it;
									
			begin -- insert_arc
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board arc
				board_reset_arc;
				board_reset_line_width;
			end insert_arc;

			procedure insert_circle (
				layer_cat	: in et_packages.type_layer_category_non_conductor;
				face		: in et_pcb_coordinates.type_face) -- TOP, BOTTOM
			is
			-- The board_circle has been a general thing until now. 
			-- Depending on the layer and the side of the board (face) the board_circle
			-- is now assigned to the board where it belongs to.

				use et_packages;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) 
				is
					use et_pcb_coordinates;
				begin
					case face is
						when TOP =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_circles.append (
										container	=> module.board.silk_screen.top.circles,
										new_item	=> board_make_fillable_circle);

								when LAYER_CAT_ASSY =>
									pac_doc_circles.append (
										container	=> module.board.assy_doc.top.circles,
										new_item	=> board_make_fillable_circle);

								when LAYER_CAT_STENCIL =>
									pac_stencil_circles.append (
										container	=> module.board.stencil.top.circles,
										new_item	=> board_make_fillable_circle);
									
								when LAYER_CAT_STOP =>
									pac_stop_circles.append (
										container	=> module.board.stop_mask.top.circles,
										new_item	=> board_make_fillable_circle);

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_circles.append (
										container	=> module.board.keepout.top.circles,
										new_item	=> board_make_fillable_circle_solid);
							end case;
							
						when BOTTOM => null;
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									pac_silk_circles.append (
										container	=> module.board.silk_screen.bottom.circles,
										new_item	=> board_make_fillable_circle);

								when LAYER_CAT_ASSY =>
									pac_doc_circles.append (
										container	=> module.board.assy_doc.bottom.circles,
										new_item	=> board_make_fillable_circle);
									
								when LAYER_CAT_STENCIL =>
									pac_stencil_circles.append (
										container	=> module.board.stencil.bottom.circles,
										new_item	=> board_make_fillable_circle);
									
								when LAYER_CAT_STOP =>
									pac_stop_circles.append (
										container	=> module.board.stop_mask.bottom.circles,
										new_item	=> board_make_fillable_circle);

								when LAYER_CAT_KEEPOUT =>
									pac_keepout_circles.append (
										container	=> module.board.keepout.bottom.circles,
										new_item	=> board_make_fillable_circle_solid);
							end case;
							
					end case;
				end do_it;
									
			begin -- insert_circle
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board circle
				board_reset_circle_fillable;
			end insert_circle;

			procedure insert_polygon (
				layer_cat	: in et_packages.type_layer_category_non_conductor;
				face		: in et_pcb_coordinates.type_face) -- TOP, BOTTOM
			is
			-- The polygon has been a general thing until now. 
			-- Depending on the layer and the side of the board (face) the polygon
			-- is now assigned to the board where it belongs to.

				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module)
				is
					use et_pcb_coordinates;
					use et_terminals;
					use et_terminals.pac_shapes;
					use et_packages;
					
					procedure append_silk_polygon_top is begin
						case board_fill_style is 
							when SOLID =>
								pac_silk_polygons.append (
									container	=> module.board.silk_screen.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style 	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_silk_polygons.append (
									container	=> module.board.silk_screen.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_silk_polygon_bottom is begin
						case board_fill_style is 
							when SOLID =>
								pac_silk_polygons.append (
									container	=> module.board.silk_screen.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style 	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_silk_polygons.append (
									container	=> module.board.silk_screen.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;
					
					procedure append_assy_doc_polygon_top is begin
						case board_fill_style is 
							when SOLID =>
								pac_doc_polygons.append (
									container	=> module.board.assy_doc.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													easing		=> board_easing,
													fill_style 	=> SOLID));

							when HATCHED =>
								pac_doc_polygons.append (
									container	=> module.board.assy_doc.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_assy_doc_polygon_bottom is begin
						case board_fill_style is 
							when SOLID =>
								pac_doc_polygons.append (
									container	=> module.board.assy_doc.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													easing		=> board_easing,
													fill_style 	=> SOLID));

							when HATCHED =>
								pac_doc_polygons.append (
									container	=> module.board.assy_doc.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with 
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_keepout_polygon_top is begin
						pac_keepout_polygons.append (
							container	=> module.board.keepout.top.polygons, 
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											filled	=> board_filled));
					end;

					procedure append_keepout_polygon_bottom is begin
						pac_keepout_polygons.append (
							container	=> module.board.keepout.bottom.polygons, 
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											filled	=> board_filled));
					end;

					procedure append_stencil_polygon_top is begin
						case board_fill_style is
							when SOLID =>
								pac_stencil_polygons.append (
									container	=> module.board.stencil.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_stencil_polygons.append (
									container	=> module.board.stencil.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_stencil_polygon_bottom is begin
						case board_fill_style is
							when SOLID =>
								pac_stencil_polygons.append (
									container	=> module.board.stencil.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_stencil_polygons.append (
									container	=> module.board.stencil.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_stop_polygon_top is begin
						case board_fill_style is
							when SOLID =>
								pac_stop_polygons.append (
									container	=> module.board.stop_mask.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_stop_polygons.append (
									container	=> module.board.stop_mask.top.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;

					procedure append_stop_polygon_bottom is begin
						case board_fill_style is
							when SOLID =>
								pac_stop_polygons.append (
									container	=> module.board.stop_mask.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> SOLID,
													easing		=> board_easing));

							when HATCHED =>
								pac_stop_polygons.append (
									container	=> module.board.stop_mask.bottom.polygons,
									new_item	=> (pac_shapes.type_polygon_base (polygon) with
													fill_style	=> HATCHED,
													easing		=> board_easing,
													hatching	=> board_hatching));
						end case;
					end;
					
				begin -- do_it
					case face is
						when TOP =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									append_silk_polygon_top;
												
								when LAYER_CAT_ASSY =>
									append_assy_doc_polygon_top;

								when LAYER_CAT_STENCIL =>
									append_stencil_polygon_top;
									
								when LAYER_CAT_STOP =>
									append_stop_polygon_top;
									
								when LAYER_CAT_KEEPOUT =>
									append_keepout_polygon_top;
									
							end case;
							
						when BOTTOM =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									append_silk_polygon_bottom;

								when LAYER_CAT_ASSY =>
									append_assy_doc_polygon_bottom;
									
								when LAYER_CAT_STENCIL =>
									append_stencil_polygon_bottom;
									
								when LAYER_CAT_STOP =>
									append_stop_polygon_bottom;
									
								when LAYER_CAT_KEEPOUT =>
									append_keepout_polygon_bottom;
									
							end case;
							
					end case;
				end do_it;
									
			begin -- insert_polygon
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board polygon
				board_reset_polygon;
			end insert_polygon;

			procedure insert_cutout (
				layer_cat	: in et_packages.type_layer_category_non_conductor;
				face		: in et_pcb_coordinates.type_face) -- TOP, BOTTOM
			is
			-- The polygon has been a general thing until now. 
			-- Depending on the layer category and the side of the board (face) the polygon
			-- is threated as a cutout zone and assigned to the board where it belongs to.

				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module)
				is
					use et_pcb_coordinates;
					use et_terminals;
					use et_terminals.pac_shapes;
					use et_packages;
					
					procedure append_silk_cutout_top is begin
						pac_silk_cutouts.append (
							container	=> module.board.silk_screen.top.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing		=> board_easing));
					end;

					procedure append_silk_cutout_bottom is begin
						pac_silk_cutouts.append (
							container	=> module.board.silk_screen.bottom.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing		=> board_easing));
					end;
					
					procedure append_assy_doc_cutout_top is begin
						pac_doc_cutouts.append (
							container	=> module.board.assy_doc.top.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
					end;

					procedure append_assy_doc_cutout_bottom is begin
						pac_doc_cutouts.append (
							container	=> module.board.assy_doc.bottom.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with 
											easing		=> board_easing));
					end;

					procedure append_keepout_cutout_top is begin
						pac_keepout_cutouts.append (
							container	=> module.board.keepout.top.cutouts, 
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;

					procedure append_keepout_cutout_bottom is begin
						pac_keepout_cutouts.append (
							container	=> module.board.keepout.bottom.cutouts, 
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;

					procedure append_stencil_cutout_top is begin
						pac_stencil_cutouts.append (
							container	=> module.board.stencil.top.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;

					procedure append_stencil_cutout_bottom is begin
						pac_stencil_cutouts.append (
							container	=> module.board.stencil.bottom.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;

					procedure append_stop_cutout_top is begin
						pac_stop_cutouts.append (
							container	=> module.board.stop_mask.top.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;

					procedure append_stop_cutout_bottom is begin
						pac_stop_cutouts.append (
							container	=> module.board.stop_mask.bottom.cutouts,
							new_item	=> (pac_shapes.type_polygon_base (polygon) with
											easing		=> board_easing));
					end;
					
				begin -- do_it
					case face is
						when TOP =>
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									append_silk_cutout_top;
												
								when LAYER_CAT_ASSY =>
									append_assy_doc_cutout_top;

								when LAYER_CAT_STENCIL =>
									append_stencil_cutout_top;
									
								when LAYER_CAT_STOP =>
									append_stop_cutout_top;
									
								when LAYER_CAT_KEEPOUT =>
									append_keepout_cutout_top;
									
							end case;
							
						when BOTTOM => null;
							case layer_cat is
								when LAYER_CAT_SILKSCREEN =>
									append_silk_cutout_bottom;

								when LAYER_CAT_ASSY =>
									append_assy_doc_cutout_bottom;
									
								when LAYER_CAT_STENCIL =>
									append_stencil_cutout_bottom;
									
								when LAYER_CAT_STOP =>
									append_stop_cutout_bottom;
									
								when LAYER_CAT_KEEPOUT =>
									append_keepout_cutout_bottom;
									
							end case;							
					end case;
				end do_it;
									
			begin -- insert_cutout
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board cutout
				board_reset_polygon;
			end insert_cutout;

			procedure insert_cutout_via_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_via_restrict_cutouts.append (
						container	=> module.board.via_restrict.cutouts,
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing	=> board_easing,
										layers	=> signal_layers));
				end do_it;
									
			begin
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board polygon
				board_reset_polygon;

				clear (signal_layers);
			end insert_cutout_via_restrict;

			procedure insert_cutout_route_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_route_restrict_cutouts.append (
						container	=> module.board.route_restrict.cutouts,
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										easing	=> board_easing,
										layers	=> signal_layers));
				end do_it;
									
			begin
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board polygon
				board_reset_polygon;

				clear (signal_layers);
			end insert_cutout_route_restrict;

			procedure insert_cutout_copper is
			-- This is about cutout zones to trim floating polygons in signal layers. No connection to any net.
				use et_terminals;
				use et_terminals.pac_shapes;
				use et_packages;
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					et_pcb.pac_conductor_cutouts.append (
						container	=> module.board.conductors.cutouts,
						new_item	=> (type_polygon_base (polygon) with
								easing			=> board_easing,
								layer			=> signal_layer));
				end do_it;
									
			begin -- insert_cutout_copper
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next floating board polygon
				board_reset_polygon;
			end insert_cutout_copper;
			
			procedure build_contour_text is

				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) 
				is
					use et_packages;
					use et_pcb;
				begin
					pac_texts_with_content.append (
						container	=> module.board.contours.texts,
						new_item	=> board_text);

				end do_it;
				
			begin -- build_contours_text
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board text
				board_text := (others => <>);

			end build_contour_text;

				
			procedure insert_placeholder (
				layer_cat	: in et_packages.type_layer_category;
				face		: in et_pcb_coordinates.type_face)  -- TOP, BOTTOM
			is
			-- The board_text_placeholder has been a general thing until now. 
			-- Depending on the layer and the side of the board (face) the board_text_placeholder
			-- is now assigned to the board where it belongs to.
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) 
				is
					use et_pcb_coordinates;
					--use et_packages;
					use et_pcb;
				begin
					case face is
						when TOP =>
							case layer_cat is
								when et_packages.LAYER_CAT_SILKSCREEN =>
									 pac_text_placeholders.append (
										container	=> module.board.silk_screen.top.placeholders,
										new_item	=> board_text_placeholder);

								when et_packages.LAYER_CAT_ASSY =>
									pac_text_placeholders.append (
										container	=> module.board.assy_doc.top.placeholders,
										new_item	=> board_text_placeholder);

								when et_packages.LAYER_CAT_STOP =>
									pac_text_placeholders.append (
										container	=> module.board.stop_mask.top.placeholders,
										new_item	=> board_text_placeholder);

								-- CS
								--when KEEPOUT =>
								--	pac_text_placeholders.append (
								--		container	=> module.board.keepout.top.placeholders,
								--		new_item	=> board_text_placeholder);

								when others => invalid_section;
							end case;
							
						when BOTTOM => null;
							case layer_cat is
								when et_packages.LAYER_CAT_SILKSCREEN =>
									pac_text_placeholders.append (
										container	=> module.board.silk_screen.bottom.placeholders,
										new_item	=> board_text_placeholder);

								when et_packages.LAYER_CAT_ASSY =>
									pac_text_placeholders.append (
										container	=> module.board.assy_doc.bottom.placeholders,
										new_item	=> board_text_placeholder);
									
								when et_packages.LAYER_CAT_STOP =>
									pac_text_placeholders.append (
										container	=> module.board.stop_mask.bottom.placeholders,
										new_item	=> board_text_placeholder);

								-- CS
								--when KEEPOUT =>
								--	pac_text_placeholders.append (
								--		container	=> module.board.keepout.bottom.placeholders,
								--		new_item	=> board_text_placeholder);

								when others => invalid_section;
							end case;
							
					end case;
				end do_it;
									
			begin -- insert_placeholder
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board placeholder
				board_text_placeholder := (others => <>);
			end insert_placeholder;

			procedure insert_line_route_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_route_restrict_lines.append (
						container	=> module.board.route_restrict.lines,
						new_item	=> (pac_shapes.type_line (board_line) with 
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_line_route_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_line;
				clear (signal_layers);
			end insert_line_route_restrict;
			
			procedure insert_arc_route_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;					
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_route_restrict_arcs.append (
						container	=> module.board.route_restrict.arcs,
						new_item	=> (pac_shapes.type_arc (board_arc) with 
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_arc_route_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_arc;

				clear (signal_layers);
			end insert_arc_route_restrict;

			procedure insert_circle_route_restrict is
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_route_restrict_circles.append (
						container	=> module.board.route_restrict.circles,
						new_item	=> (board_make_fillable_circle_solid with signal_layers));
				end do_it;
									
			begin -- insert_circle_route_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_circle;
				clear (signal_layers);
			end insert_circle_route_restrict;

			procedure insert_polygon_route_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_route_restrict_polygons.append (
						container	=> module.board.route_restrict.polygons,
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_polygon_route_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board polygon
				board_reset_polygon;

				clear (signal_layers);
			end insert_polygon_route_restrict;

			procedure insert_line_via_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_via_restrict_lines.append (
						container	=> module.board.via_restrict.lines,
						new_item	=> (pac_shapes.type_line (board_line) with 
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_line_via_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_line;

				clear (signal_layers);
			end insert_line_via_restrict;

			procedure insert_arc_via_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_via_restrict_arcs.append (
						container	=> module.board.via_restrict.arcs,
						new_item	=> (pac_shapes.type_arc (board_arc) with 
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_arc_via_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_arc;

				clear (signal_layers);
			end insert_arc_via_restrict;

			procedure insert_circle_via_restrict is
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_via_restrict_circles.append (
						container	=> module.board.via_restrict.circles,
						new_item	=> (board_make_fillable_circle_solid with signal_layers));
				end do_it;
									
			begin -- insert_circle_via_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board line
				board_reset_circle;

				clear (signal_layers);
			end insert_circle_via_restrict;

			procedure insert_polygon_via_restrict is
				use et_terminals;
				use et_packages;
				use et_pcb_stack;
				use type_signal_layers;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_via_restrict_polygons.append (
						container	=> module.board.via_restrict.polygons,
						new_item	=> (pac_shapes.type_polygon_base (polygon) with 
										filled	=> board_filled,
										layers	=> signal_layers));
				end do_it;
									
			begin -- insert_polygon_via_restrict
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next board polygon
				board_reset_polygon;

				clear (signal_layers);
			end insert_polygon_via_restrict;

			procedure insert_polygon_copper is
			-- This is about floating polygons in signal layers. No connection to any net.
				use et_packages;
				use et_terminals.pac_shapes;
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					case board_fill_style is
						when SOLID =>
							pac_conductor_polygons_floating_solid.append (
								container	=> module.board.conductors.polygons.solid,
								new_item	=> (type_polygon_base (polygon) with
										fill_style 		=> SOLID,
										easing			=> board_easing,
										priority_level	=> polygon_priority,
										isolation		=> polygon_isolation,
										layer			=> signal_layer,
										width_min		=> polygon_width_min)
										);

						when HATCHED =>
							pac_conductor_polygons_floating_hatched.append (
								container	=> module.board.conductors.polygons.hatched,
								new_item	=> (type_polygon_base (polygon) with
										fill_style 		=> HATCHED,
										easing			=> board_easing,
										priority_level	=> polygon_priority,
										isolation		=> polygon_isolation,
										layer			=> signal_layer,
										width_min		=> polygon_width_min,
										hatching		=> board_hatching)
										);
					end case;
				end do_it;
									
			begin -- insert_polygon_copper
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next floating board polygon
				board_reset_polygon;
			end insert_polygon_copper;

			procedure insert_line_track is -- about freetracks
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_conductor_lines.append (
						container	=> module.board.conductors.lines,
						new_item	=> (et_terminals.pac_shapes.type_line (board_line) with
										width	=> board_line_width,
										layer	=> signal_layer));
				end;
									
			begin -- insert_line_track
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next track line
				board_reset_line;
				board_reset_line_width;
				board_reset_signal_layer;
			end insert_line_track;

			procedure insert_arc_track is -- about freetracks
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_conductor_arcs.append (
						container	=> module.board.conductors.arcs,
						new_item	=> (et_terminals.pac_shapes.type_arc (board_arc) with
										width	=> board_line_width,
										layer	=> signal_layer));
				end;
									
			begin -- insert_arc_track
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next track arc
				board_reset_arc;
				board_reset_line_width;
				board_reset_signal_layer;
			end insert_arc_track;

			procedure insert_circle_track is -- about freetracks
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_conductor_circles.append (
						container	=> module.board.conductors.circles,
						new_item	=> (board_make_copper_circle with signal_layer));
				end;
									
			begin -- insert_circle_track
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next track circle
				board_reset_circle;
				board_reset_line_width;
				board_reset_signal_layer;
				-- CS reset other properites
			end insert_circle_track;

			procedure insert_board_text is
				use et_packages;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_conductor_texts.append (
						container	=> module.board.conductors.texts,
						new_item	=> board_text_copper);
				end do_it;
									
			begin -- insert_board_text
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next text in copper
				board_text_copper := (others => <>);
			end insert_board_text;

			procedure insert_board_text_placeholder is
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_text_placeholders_conductors.append (
						container	=> module.board.conductors.placeholders,
						new_item	=> board_text_copper_placeholder);
				end do_it;
									
			begin -- insert_board_text_placeholder
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next placeholder in copper
				board_text_copper_placeholder := (others => <>);
			end insert_board_text_placeholder;
			
			procedure insert_line_contour is
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_pcb_contour_lines.append (
						container	=> module.board.contours.lines,
						new_item	=> (et_terminals.pac_shapes.type_line (board_line) with board_lock_status));
				end do_it;
									
			begin -- insert_line_contour
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next pcb contour line
				board_reset_line;
				board_reset_lock_status;
			end insert_line_contour;
			
			procedure insert_arc_contour is
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_pcb_contour_arcs.append (
						container	=> module.board.contours.arcs,
						new_item	=> (et_terminals.pac_shapes.type_arc (board_arc) with board_lock_status));
				end do_it;
									
			begin -- insert_arc_contour
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next pcb contour arc
				board_reset_arc;
				board_reset_lock_status;
			end insert_arc_contour;

			procedure insert_circle_contour is
				use et_pcb;
				
				procedure do_it (
					module_name	: in pac_module_name.bounded_string;
					module		: in out et_schematic.type_module) is
				begin
					pac_pcb_contour_circles.append (
						container	=> module.board.contours.circles,
						new_item	=> (et_terminals.pac_shapes.type_circle (board_circle) with board_lock_status));
				end do_it;
									
			begin -- insert_circle_contour
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> do_it'access);

				-- clean up for next pcb contour circle
				board_reset_circle;
				board_reset_lock_status;
			end insert_circle_contour;

			--procedure insert_text_contour is
				--use et_pcb;
				
				--procedure do_it (
					--module_name	: in pac_module_name.bounded_string;
					--module		: in out et_schematic.type_module) is
				--begin
					--pac_pcb_contour_circles.append (
						--container	=> module.board.contours.texts,
						--new_item	=> (et_terminals.pac_shapes.type_circle (board_circle) with board_lock_status));
				--end do_it;
									
			--begin -- insert_text_contour
				--update_element (
					--container	=> generic_modules,
					--position	=> module_cursor,
					--process		=> do_it'access);

				---- clean up for next pcb contour circle
				--board_reset_circle;
				--board_reset_lock_status;
			--end insert_text_contour;

			procedure build_non_conductor_line (
				face : in et_pcb_coordinates.type_face)
			is
				use et_packages;
			begin
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_line (
							layer_cat	=> LAYER_CAT_SILKSCREEN,
							face		=> face);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_line (
							layer_cat	=> LAYER_CAT_ASSY,
							face		=> face);

					when SEC_STENCIL =>
						insert_line (
							layer_cat	=> LAYER_CAT_STENCIL,
							face		=> face);

					when SEC_STOP_MASK =>
						insert_line (
							layer_cat	=> LAYER_CAT_STOP,
							face		=> face);

					when SEC_KEEPOUT =>
						insert_line (
							layer_cat	=> LAYER_CAT_KEEPOUT,
							face		=> face);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_line;

			procedure build_non_conductor_arc (
				face : in et_pcb_coordinates.type_face)
			is
				use et_packages;
			begin
				board_check_arc (log_threshold + 1);
				
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_arc (
							layer_cat	=> LAYER_CAT_SILKSCREEN,
							face		=> face);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_arc (
							layer_cat	=> LAYER_CAT_ASSY,
							face		=> face);

					when SEC_STENCIL =>
						insert_arc (
							layer_cat	=> LAYER_CAT_STENCIL,
							face		=> face);

					when SEC_STOP_MASK =>
						insert_arc (
							layer_cat	=> LAYER_CAT_STOP,
							face		=> face);

					when SEC_KEEPOUT =>
						insert_arc (
							layer_cat	=> LAYER_CAT_KEEPOUT,
							face		=> face);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_arc;

			procedure build_non_conductor_circle (
				face : in et_pcb_coordinates.type_face)
			is
				use et_packages;
			begin
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_circle (
							layer_cat	=> LAYER_CAT_SILKSCREEN,
							face		=> face);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_circle (
							layer_cat	=> LAYER_CAT_ASSY,
							face		=> face);

					when SEC_STENCIL =>
						insert_circle (
							layer_cat	=> LAYER_CAT_STENCIL,
							face		=> face);

					when SEC_STOP_MASK =>
						insert_circle (
							layer_cat	=> LAYER_CAT_STOP,
							face		=> face);

					when SEC_KEEPOUT =>
						insert_circle (
							layer_cat	=> LAYER_CAT_KEEPOUT,
							face		=> face);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_circle;							
			
			procedure insert_netchanger (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				inserted : boolean;
				use et_submodules;
				use pac_netchangers;
				cursor : pac_netchangers.cursor;
			begin
				log (text => "netchanger " & to_string (netchanger_id), level => log_threshold + 2);

				-- insert netchanger in container netchangers:
				insert (
					container	=> module.netchangers,
					key			=> netchanger_id,
					new_item	=> netchanger,
					inserted	=> inserted,
					position	=> cursor);

				-- A netchanger name must be unique:
				if not inserted then
					log (ERROR, "netchanger id" & to_string (netchanger_id) 
						& " already used !", console => true);
					raise constraint_error;
				end if;
				
				-- clean up for next netchanger
				netchanger_id := type_netchanger_id'first;
				netchanger := (others => <>);
			end insert_netchanger;

			procedure insert_assembly_variant (
				module_name	: in pac_module_name.bounded_string;
				module		: in out et_schematic.type_module) is
				inserted : boolean;
				use et_assembly_variants;
				use et_assembly_variants.pac_assembly_variants;
				cursor : et_assembly_variants.pac_assembly_variants.cursor;
			begin
				log (text => "assembly variant " & 
						enclose_in_quotes (to_variant (assembly_variant_name)), level => log_threshold + 2);

				-- insert variant in container variants
				et_assembly_variants.pac_assembly_variants.insert (
					container	=> module.variants,
					key			=> assembly_variant_name,
					inserted	=> inserted,
					position	=> cursor,
					new_item	=> (
						description	=> assembly_variant_description,
						devices		=> assembly_variant_devices,
						submodules	=> assembly_variant_submodules));

				-- An assembly variant must be unique:
				if not inserted then
					log (ERROR, "assembly variant " & 
							enclose_in_quotes (to_variant (assembly_variant_name)) 
							& " already used !", console => true);
					raise constraint_error;
				end if;

				-- clean up for next assembly variant
				assembly_variant_name := to_variant ("");
				assembly_variant_description := to_unbounded_string ("");
				assembly_variant_devices := et_assembly_variants.pac_device_variants.empty_map;
				assembly_variant_submodules := pac_submodule_variants.empty_map;
				
			end insert_assembly_variant;

			procedure build_route_polygon is
				use et_terminals.pac_shapes;

				procedure solid_polygon is
					use et_pcb.pac_signal_polygons_solid;
					use et_packages;

					procedure connection_thermal is
						p : et_pcb.type_conductor_polygon_solid (connection => et_pcb.THERMAL);
					begin
						p.segments := polygon.segments;
						
						p.easing := board_easing;
						
						p.width_min	:= polygon_width_min;
						p.isolation	:= polygon_isolation;
						
						p.layer				:= signal_layer;
						p.priority_level	:= polygon_priority;
						p.thermal			:= thermal;

						et_pcb.pac_signal_polygons_solid.append (
							container	=> route.polygons_2.solid,
							new_item	=> p);
					end;

					procedure connection_solid is
						p : et_pcb.type_conductor_polygon_solid (connection => et_pcb.SOLID);
					begin
						p.segments := polygon.segments;
						
						p.easing := board_easing;
						
						p.width_min	:= polygon_width_min;
						p.isolation	:= polygon_isolation;
						
						p.layer				:= signal_layer;
						p.priority_level	:= polygon_priority;
						
						p.technology := thermal.technology;

						et_pcb.pac_signal_polygons_solid.append (
							container	=> route.polygons_2.solid,
							new_item	=> p);
					end;
					
				begin -- solid_polygon
					case polygon_pad_connection is
						when et_pcb.THERMAL => connection_thermal;
						when et_pcb.SOLID => connection_solid;
					end case;
				end solid_polygon;

				procedure hatched_polygon is
					use et_pcb.pac_signal_polygons_hatched;
					use et_packages;

					procedure connection_thermal is
						p : et_pcb.type_conductor_polygon_hatched (connection => et_pcb.THERMAL);
					begin
						p.segments := polygon.segments;
						
						p.easing := board_easing;
						
						p.width_min	:= polygon_width_min;
						p.isolation	:= polygon_isolation;
						
						p.layer				:= signal_layer;
						p.priority_level	:= polygon_priority;
						p.thermal			:= thermal;
						
						et_pcb.pac_signal_polygons_hatched.append (
							container	=> route.polygons_2.hatched,
							new_item	=> p);
					end;

					procedure connection_solid is
						p : et_pcb.type_conductor_polygon_hatched (connection => et_pcb.SOLID);
					begin
						p.segments := polygon.segments;
						
						p.easing := board_easing;
						
						p.width_min	:= polygon_width_min;
						p.isolation	:= polygon_isolation;
						
						p.layer				:= signal_layer;
						p.priority_level	:= polygon_priority;
						
						p.technology := thermal.technology;
						
						et_pcb.pac_signal_polygons_hatched.append (
							container	=> route.polygons_2.hatched,
							new_item	=> p);
					end;
					
				begin -- hatched_polygon
					case polygon_pad_connection is
						when et_pcb.THERMAL => connection_thermal;
						when et_pcb.SOLID => connection_solid;
					end case;
				end hatched_polygon;

			begin -- build_route_polygon
				case board_fill_style is
					when et_packages.SOLID		=> solid_polygon;
					when et_packages.HATCHED	=> hatched_polygon;
				end case;

				board_reset_polygon; -- clean up for next polygon
			end build_route_polygon;

			procedure build_route_cutout is
				use et_terminals;
			begin
				et_pcb.pac_conductor_cutouts.append (
					container	=> route.cutouts,
					new_item	=> (pac_shapes.type_polygon_base (polygon) with
									easing	=> board_easing,
									layer	=> signal_layer));

				board_reset_polygon; -- clean up for next cutout zone
			end build_route_cutout;

			procedure build_non_conductor_cutout (
				face	: in et_pcb_coordinates.type_face) 
			is 
				use et_packages;
			begin
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_cutout (
							layer_cat	=> LAYER_CAT_SILKSCREEN,
							face		=> face);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_cutout (
							layer_cat	=> LAYER_CAT_ASSY,
							face		=> face);

					when SEC_STENCIL =>
						insert_cutout (
							layer_cat	=> LAYER_CAT_STENCIL,
							face		=> face);

					when SEC_STOP_MASK =>
						insert_cutout (
							layer_cat	=> LAYER_CAT_STOP,
							face		=> face);

					when SEC_KEEPOUT =>
						insert_cutout (
							layer_cat	=> LAYER_CAT_KEEPOUT,
							face		=> face);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_cutout;

			procedure build_non_conductor_fill_zone (
				face	: in et_pcb_coordinates.type_face)
			is
				use et_packages;
			begin
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_polygon (
							layer_cat	=> LAYER_CAT_SILKSCREEN,
							face		=> face);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_polygon (
							layer_cat	=> LAYER_CAT_ASSY,
							face		=> face);

					when SEC_STENCIL =>
						insert_polygon (
							layer_cat	=> LAYER_CAT_STENCIL,
							face		=> face);

					when SEC_STOP_MASK =>
						insert_polygon (
							layer_cat	=> LAYER_CAT_STOP,
							face		=> face);

					when SEC_KEEPOUT =>
						insert_polygon (
							layer_cat	=> LAYER_CAT_KEEPOUT,
							face		=> face);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_fill_zone;
			
			procedure build_non_conductor_text (
				face : in et_pcb_coordinates.type_face)  -- TOP, BOTTOM
			is
			-- The board_text has been a general thing until now. 
			-- Depending on the layer category and the side of the board (face) the board_text
			-- is now assigned to the board where it belongs to.
				use et_packages;

				procedure insert_text (
					layer_cat	: in et_packages.type_layer_category_non_conductor)
				is					
					procedure do_it (
						module_name	: in pac_module_name.bounded_string;
						module		: in out et_schematic.type_module) 
					is
						use et_pcb_coordinates;
						use et_packages;
						use et_pcb;
					begin
						case face is
							when TOP =>
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_texts_with_content.append (
											container	=> module.board.silk_screen.top.texts,
											new_item	=> board_text);

									when LAYER_CAT_ASSY =>
										pac_texts_with_content.append (
											container	=> module.board.assy_doc.top.texts,
											new_item	=> board_text);

									when LAYER_CAT_KEEPOUT =>
										pac_texts_with_content.append (
											container	=> module.board.keepout.top.texts,
											new_item	=> board_text);

									when LAYER_CAT_STENCIL =>
										pac_texts_with_content.append (
											container	=> module.board.stencil.top.texts,
											new_item	=> board_text);

									when LAYER_CAT_STOP =>
										pac_texts_with_content.append (
											container	=> module.board.stop_mask.top.texts,
											new_item	=> board_text);

									when others => invalid_section;
								end case;
								
							when BOTTOM => null;
								case layer_cat is
									when LAYER_CAT_SILKSCREEN =>
										pac_texts_with_content.append (
											container	=> module.board.silk_screen.bottom.texts,
											new_item	=> board_text);

									when LAYER_CAT_ASSY =>
										pac_texts_with_content.append (
											container	=> module.board.assy_doc.bottom.texts,
											new_item	=> board_text);
										
									when LAYER_CAT_KEEPOUT =>
										pac_texts_with_content.append (
											container	=> module.board.keepout.bottom.texts,
											new_item	=> board_text);

									when LAYER_CAT_STENCIL =>
										pac_texts_with_content.append (
											container	=> module.board.stencil.bottom.texts,
											new_item	=> board_text);

									when LAYER_CAT_STOP =>
										pac_texts_with_content.append (
											container	=> module.board.stop_mask.bottom.texts,
											new_item	=> board_text);

									when others => invalid_section;
								end case;
								
						end case;
					end do_it;
										
				begin -- insert_text
					update_element (
						container	=> generic_modules,
						position	=> module_cursor,
						process		=> do_it'access);

					-- clean up for next board text
					board_text := (others => <>);
				end insert_text;
				
			begin -- build_non_conductor_text
				case stack.parent (degree => 2) is
					when SEC_SILK_SCREEN =>
						insert_text (LAYER_CAT_SILKSCREEN);

					when SEC_ASSEMBLY_DOCUMENTATION =>
						insert_text (LAYER_CAT_ASSY);

					when SEC_KEEPOUT =>
						insert_text (LAYER_CAT_KEEPOUT);

					when SEC_STENCIL =>
						insert_text (LAYER_CAT_STENCIL);
						
					when SEC_STOP_MASK =>
						insert_text (LAYER_CAT_STOP);
						
					when others => invalid_section;
				end case;
			end build_non_conductor_text;
			
		begin -- execute_section
			case stack.current is

				when SEC_CONTOURS =>
					case stack.parent is
						when SEC_FILL_ZONE => check_outline (polygon, log_threshold + 1);
						when SEC_CUTOUT_ZONE => check_outline (polygon, log_threshold + 1);
						
						when others => invalid_section;
					end case;
				
				when SEC_NET_CLASS =>
					case stack.parent is
						when SEC_NET_CLASSES =>

							-- insert net class
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_net_class'access);
							
						when others => invalid_section;
					end case;

				when SEC_NET_CLASSES =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;

				when SEC_BOARD_LAYER_STACK =>
					case stack.parent is
						when SEC_INIT => 

							-- add board layer
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> add_board_layer'access);

							-- Now that the board layer stack is complete,
							-- we assign the deepest layer to check_layers.
							check_layers.deepest_layer := 
								et_pcb_stack.deepest_layer (element (module_cursor).board.stack);
							
						when others => invalid_section;
					end case;
					
				when SEC_DRAWING_GRID =>
					case stack.parent is
						when SEC_INIT => set_drawing_grid;
						when others => invalid_section;
					end case;
					
				when SEC_NET =>
					case stack.parent is
						when SEC_NETS =>

							-- insert net
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_net'access);

						when others => invalid_section;
					end case;

				when SEC_NETS =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;
					
				when SEC_STRANDS =>
					case stack.parent is
						when SEC_NET =>

							-- insert strand collection in net
							net.strands := strands;
							et_schematic.pac_strands.clear (strands); -- clean up for next strand collection

						when others => invalid_section;
					end case;

				when SEC_ROUTE =>
					case stack.parent is
						when SEC_NET =>

							-- insert route in net
							net.route := route;
							route := (others => <>); -- clean up route for next net
							
						when others => invalid_section;
					end case;

				when SEC_STRAND =>
					case stack.parent is
						when SEC_STRANDS =>

							declare
								use et_coordinates;
								use pac_geometry_sch;
								position_found_in_module_file : type_point := type_point (strand.position);
							begin
								-- Calculate the lowest x/y position and set sheet number of the strand
								-- and overwrite previous x/y position. 
								-- So the calculated position takes precedence over the position found in 
								-- the module file.
								et_schematic.set_strand_position (strand);

								-- Issue warning about this mismatch:
								if type_point (strand.position) /= position_found_in_module_file then
									log (WARNING, affected_line (line) & "Sheet" & to_sheet (sheet (strand.position))
										 & " net " 
										 & et_general.to_string (net_name) & ": Lowest x/y position of strand invalid !");
									log (text => " Found " & to_string (point => position_found_in_module_file));
									log (text => " Will be overridden by calculated position" & 
											to_string (point => type_point (strand.position)));
								end if;
							end;
							
							-- insert strand in collection of strands
							et_schematic.pac_strands.append (
								container	=> strands,
								new_item	=> strand);

							-- clean up for next single strand
							strand := (others => <>); 
							
						when others => invalid_section;
					end case;

				when SEC_SEGMENTS =>
					case stack.parent is
						when SEC_STRAND =>

							-- insert segments in strand
							strand.segments := net_segments;

							-- clean up for next segment collection
							et_schematic.pac_net_segments.clear (net_segments);
							
						when others => invalid_section;
					end case;

				when SEC_SEGMENT =>
					case stack.parent is
						when SEC_SEGMENTS =>

							-- Copy the net_junctions into the segment.
							net_segment.junctions := net_junctions;

							-- Reset net_junctions for next net segment.
							net_junctions := (others => <>);
							
							-- insert segment in segment collection
							et_schematic.pac_net_segments.append (
								container	=> net_segments,
								new_item	=> net_segment);

							-- clean up for next segment
							net_segment := (others => <>);
							
						when others => invalid_section;
					end case;

				when SEC_LABELS =>
					case stack.parent is
						when SEC_SEGMENT =>

							-- insert labels in segment
							net_segment.labels := net_labels;

							-- clean up for next label collection
							et_schematic.pac_net_labels.clear (net_labels);

						when others => invalid_section;
					end case;

				when SEC_PORTS =>
					case stack.parent is
						when SEC_SEGMENT =>

							-- NOTE: A device, submodule or netchanger port is defined by a
							-- single line.
							-- Upon reading the a line like 
							--   "device/submodule/netchanger x port 1/4/slave/master" 
							-- the port is appended to the corresponding port collection 
							-- immediately when the line is read. See main code of process_line.
							-- There is no section for a single port like [PORT BEGIN].

							-- insert port collection in segment
							net_segment.ports_devices := net_device_ports;

							-- insert submodule ports in segment
							net_segment.ports_submodules := net_submodule_ports;

							-- insert netchanger ports in segment
							net_segment.ports_netchangers := net_netchanger_ports;
							
							-- clean up for next port collections (of another net segment)
							et_schematic.pac_device_ports.clear (net_device_ports);
							et_schematic.pac_submodule_ports.clear (net_submodule_ports);
							et_netlists.pac_netchanger_ports.clear (net_netchanger_ports);

						when SEC_SUBMODULE =>
							-- copy collection of ports to submodule
							submodule.ports := submodule_ports;

							-- clean up for next collection of ports
							et_submodules.pac_submodule_ports.clear (submodule_ports);
							
						when others => invalid_section;
					end case;
							
				when SEC_LABEL =>
					case stack.parent is
						when SEC_LABELS =>

							-- insert label in label collection
							case net_label_appearance is
								when et_schematic.SIMPLE =>

									-- insert a simple label
									et_schematic.pac_net_labels.append (
										container	=> net_labels,
										new_item	=> (
											net_label with
											appearance		=> et_schematic.SIMPLE,
											rotation_simple	=> snap (net_label_rotation)
											));

									-- CS warn about parameter "direction" being ignored
									
								when et_schematic.TAG =>

									-- insert a tag label
									et_schematic.pac_net_labels.append (
										container	=> net_labels,
										new_item	=> (
											net_label with 
											appearance		=> et_schematic.TAG,
											direction		=> net_label_direction,
											rotation_tag	=> net_label_rotation
											));

							end case;

							-- clean up for next label
							net_label := (others => <>);
							net_label_rotation := et_coordinates.pac_geometry_sch.zero_rotation;
							net_label_appearance := et_schematic.type_net_label_appearance'first;
							net_label_direction := et_schematic.type_net_label_direction'first;

						when others => invalid_section;
					end case;
					
				when SEC_LINE =>
					case stack.parent is
						when SEC_CONTOURS => add_polygon_line (board_line);
							
						when SEC_ROUTE =>

							-- insert line in route.lines
							et_pcb.pac_conductor_lines.append (
								container	=> route.lines,
								new_item	=> (et_terminals.pac_shapes.type_line (board_line) with
										width	=> board_line_width,
										layer	=> signal_layer));
								
							board_reset_line;
							board_reset_line_width;
							board_reset_signal_layer;

						when SEC_TOP =>
							build_non_conductor_line (et_pcb_coordinates.TOP);

						when SEC_BOTTOM =>
							build_non_conductor_line (et_pcb_coordinates.BOTTOM);

						when SEC_ROUTE_RESTRICT =>
							insert_line_route_restrict;

						when SEC_VIA_RESTRICT =>
							insert_line_via_restrict;

						when SEC_COPPER =>
							insert_line_track;

						when SEC_PCB_CONTOURS_NON_PLATED =>
							insert_line_contour;
							
						when others => invalid_section;
					end case;
					
				when SEC_ARC =>
					case stack.parent is
						when SEC_CONTOURS => 
							board_check_arc (log_threshold + 1);
							add_polygon_arc (board_arc);

						when SEC_ROUTE =>
							board_check_arc (log_threshold + 1);
							
							-- insert arc in route.arcs
							et_pcb.pac_conductor_arcs.append (
								container	=> route.arcs,
								new_item	=> (et_terminals.pac_shapes.type_arc (board_arc) with
										width	=> board_line_width,
										layer	=> signal_layer));
								
							board_reset_arc;
							board_reset_line_width;
							board_reset_signal_layer;
							
						when SEC_TOP =>
							build_non_conductor_arc (et_pcb_coordinates.TOP);

						when SEC_BOTTOM =>
							build_non_conductor_arc (et_pcb_coordinates.BOTTOM);

						when SEC_ROUTE_RESTRICT =>
							board_check_arc (log_threshold + 1);
							insert_arc_route_restrict;

						when SEC_VIA_RESTRICT =>
							board_check_arc (log_threshold + 1);
							insert_arc_via_restrict;

						when SEC_COPPER =>
							board_check_arc (log_threshold + 1);
							insert_arc_track;

						when SEC_PCB_CONTOURS_NON_PLATED =>
							board_check_arc (log_threshold + 1);
							insert_arc_contour;
							
						when others => invalid_section;
					end case;

				when SEC_CIRCLE =>
					case stack.parent is
						when SEC_CONTOURS => add_polygon_circle (board_circle);
						
						when SEC_TOP =>
							build_non_conductor_circle (et_pcb_coordinates.TOP);

						when SEC_BOTTOM =>
							build_non_conductor_circle (et_pcb_coordinates.BOTTOM);

						when SEC_ROUTE_RESTRICT =>
							insert_circle_route_restrict;

						when SEC_VIA_RESTRICT =>
							insert_circle_via_restrict;

						when SEC_COPPER =>
							insert_circle_track;

						when SEC_PCB_CONTOURS_NON_PLATED =>
							insert_circle_contour;
							
						when others => invalid_section;
					end case;
					
				when SEC_VIA =>
					case stack.parent is
						when SEC_ROUTE =>

							-- insert via in route.vias
							et_pcb.pac_vias.append (route.vias, route_via);
							route_via := (others => <>); -- clean up for next via

						when others => invalid_section;
					end case;

				when SEC_CUTOUT_ZONE =>
					case stack.parent is
						when SEC_ROUTE =>
							build_route_cutout;

						when SEC_TOP =>
							build_non_conductor_cutout (et_pcb_coordinates.TOP);

						when SEC_BOTTOM =>
							build_non_conductor_cutout (et_pcb_coordinates.BOTTOM);

						when SEC_ROUTE_RESTRICT =>
							insert_cutout_route_restrict;

						when SEC_VIA_RESTRICT =>
							insert_cutout_via_restrict;

						when SEC_COPPER =>
							insert_cutout_copper;
							
						when others => invalid_section;
					end case;
					
				when SEC_FILL_ZONE =>
					case stack.parent is
						when SEC_ROUTE =>
							build_route_polygon;

						when SEC_TOP =>
							build_non_conductor_fill_zone (et_pcb_coordinates.TOP);
					
						when SEC_BOTTOM =>
							build_non_conductor_fill_zone (et_pcb_coordinates.BOTTOM);

						when SEC_ROUTE_RESTRICT =>
							insert_polygon_route_restrict;

						when SEC_VIA_RESTRICT =>
							insert_polygon_via_restrict;

						when SEC_COPPER =>
							insert_polygon_copper;
							
						when others => invalid_section;
					end case;

				when SEC_SUBMODULE =>
					case stack.parent is
						when SEC_SUBMODULES =>

							-- insert submodule
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_submodule'access);

						when others => invalid_section;
					end case;

				when SEC_PORT =>
					case stack.parent is
						when SEC_PORTS =>
							case stack.parent (degree => 2) is
								when SEC_SUBMODULE =>
									declare
										cursor : et_submodules.pac_submodule_ports.cursor;
										inserted : boolean;
									begin
										-- Test whether the port sits at the edge of the submodule box:
										if et_submodules.at_edge (submodule_port.position, submodule.size) then
											
											-- append port to collection of submodule ports
											et_submodules.pac_submodule_ports.insert (
												container	=> submodule_ports,
												key			=> submodule_port_name, -- RESET
												new_item	=> submodule_port,
												inserted	=> inserted,
												position	=> cursor
												);

											if not inserted then
												log (ERROR, "port " & 
													et_general.to_string (submodule_port_name) & " already used !",
													console => true
													);
												raise constraint_error;
											end if;

										else
											port_not_at_edge (submodule_port_name);
										end if;
									end;

									-- clean up for next port
									submodule_port_name := to_net_name ("");
									submodule_port := (others => <>);

								when others => invalid_section;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_SUBMODULES =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;

				when SEC_SCHEMATIC =>
					case stack.parent is
						when SEC_DRAWING_FRAMES =>

							-- set schematic frame template
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> set_frame_schematic'access);

						when SEC_DRAWING_GRID => null; -- nothing to do

						when SEC_META =>
							-- Add the so far collected basic meta data AND the 
							-- preferred schematic libs to schematic related meta data:
							meta_schematic := (meta_basic with 
								preferred_libs => prf_libs_sch);

							-- This clean up is not really required since
							-- section meta and preferred libs for schematic
							-- exist only once in the module file:
							prf_libs_sch.clear;
							
							-- Clean up basic meta stuff because
							-- it will be used for the board also:
							meta_basic := (others => <>);

						when others => invalid_section;
					end case;

				when SEC_BOARD =>
					case stack.parent is
						when SEC_INIT => null;

						when SEC_DRAWING_FRAMES =>
							
							-- set board/layout frame template
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> set_frame_board'access);

						when SEC_DRAWING_GRID => null; -- nothing to do

						when SEC_META =>
							-- Add the so far collected basic meta data AND the 
							-- preferred board libs to board related meta data:
							meta_board := (meta_basic with
								preferred_libs => prf_libs_brd);

							-- This clean up is not really required since
							-- section meta and preferred libs for board
							-- exist only once in the module file:
							prf_libs_brd.clear;
							
							-- Clean up basic meta stuff because
							-- it will be used for the schematic also:
							meta_basic := (others => <>);
						
						when others => invalid_section;
					end case;

				when SEC_SHEET_DESCRIPTIONS =>
					case stack.parent is
						when SEC_SCHEMATIC => null; 
							-- We assign the sheet_descriptions once parent 
							-- section SCHEMATIC closes.
							-- See procdure set_frame_schematic.

						when others => invalid_section;
					end case;

				when SEC_SHEET =>
					case stack.parent is
						when SEC_SHEET_DESCRIPTIONS => add_sheet_description;
						when others => invalid_section;
					end case;
					
				when SEC_TEXT =>
					case stack.parent is
						when SEC_TEXTS =>

							-- insert note
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_note'access);

						when SEC_TOP =>
							build_non_conductor_text (et_pcb_coordinates.TOP);
					
						when SEC_BOTTOM =>
							build_non_conductor_text (et_pcb_coordinates.BOTTOM);

						when SEC_PCB_CONTOURS_NON_PLATED =>
							build_contour_text;
							
						when SEC_COPPER =>
							insert_board_text;
							
						when others => invalid_section;
					end case;

				when SEC_TEXTS =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;

				when SEC_DRAWING_FRAMES =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;
					
				when SEC_PLACEHOLDER =>
					case stack.parent is
						when SEC_PLACEHOLDERS =>
							case stack.parent (degree => 2) is
								when SEC_DEVICE | SEC_PACKAGE =>

									-- insert package placeholder in collection of text placeholders
									insert_package_placeholder;

								when SEC_UNIT =>

									-- build temporarily unit placeholder
									build_unit_placeholder;

								when others => invalid_section;
							end case;

						when SEC_TOP =>
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_SILKSCREEN,
										face		=> et_pcb_coordinates.TOP);

								when SEC_ASSEMBLY_DOCUMENTATION =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_ASSY,
										face		=> et_pcb_coordinates.TOP);

								when SEC_STOP_MASK =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_STOP,
										face		=> et_pcb_coordinates.TOP);

								when others => invalid_section;
							end case;
							
						when SEC_BOTTOM =>
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_SILKSCREEN,
										face		=> et_pcb_coordinates.BOTTOM);

								when SEC_ASSEMBLY_DOCUMENTATION =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_ASSY,
										face		=> et_pcb_coordinates.BOTTOM);

								when SEC_STOP_MASK =>
									insert_placeholder (
										layer_cat	=> et_packages.LAYER_CAT_STOP,
										face		=> et_pcb_coordinates.BOTTOM);

								when others => invalid_section;
							end case;

						when SEC_COPPER =>
							insert_board_text_placeholder;
							
						when others => invalid_section;
					end case;

				when SEC_PLACEHOLDERS =>
					case stack.parent is
						when SEC_PACKAGE =>

							-- Insert placeholder collection in temporarily device:
							-- CS: constraint error will arise here if the device is virtual.
							-- issue warning and skip this statement in this case:
							device.text_placeholders :=	device_text_placeholders;

							-- clean up for next collection of placeholders
							device_text_placeholders := (others => <>);

						when SEC_DEVICE =>
							case stack.parent (degree => 2) is
								when SEC_DEVICES_NON_ELECTRIC =>
									
									-- Insert placeholder collection in temporarily device:
									device_non_electric.text_placeholders := device_text_placeholders;

									-- clean up for next collection of placeholders
									device_text_placeholders := (others => <>);

								when others => invalid_section;
							end case;
							
						when SEC_UNIT => null;
							
						when others => invalid_section;
					end case;

				when SEC_PACKAGE =>
					case stack.parent is
						when SEC_DEVICE =>

							-- Assign coordinates of package to temporarily device:
							-- CS: constraint error will arise here if the device is virtual.
							-- issue warning and skip this statement in this case:
							device.position := device_position;

							-- Assign flipped flag
							device.flipped := device_flipped;

							-- reset device package position for next device
							device_position := et_pcb_coordinates.package_position_default;

							-- reset flip flag for next device
							device_flipped := et_pcb.flipped_default;

						when others => invalid_section;
					end case;

				when SEC_UNIT =>
					case stack.parent is
						when SEC_UNITS =>

							-- insert unit in temporarily collection of units
							insert_unit;
												
						when others => invalid_section;
					end case;

				when SEC_UNITS =>
					case stack.parent is
						when SEC_DEVICE =>

							-- insert temporarily collection of units in device
							device.units := device_units;

							-- clear temporarily collection of units for next device
							et_schematic.pac_units.clear (device_units);
							
						when others => invalid_section;
					end case;

				when SEC_DEVICE =>
					case stack.parent is
						when SEC_DEVICES =>

							-- insert device (where pointer "device" is pointing at) in the module
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_device'access);

						when SEC_DEVICES_NON_ELECTRIC => 

							-- insert device (where pointer "device_non_electric" is pointing at) in the module
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_device_non_electric'access);
							
						when others => invalid_section;
					end case;

				when SEC_DEVICES =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;

				when SEC_ASSEMBLY_VARIANT =>
					case stack.parent is
						when SEC_ASSEMBLY_VARIANTS => 

							-- insert the assembly variant in the module
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_assembly_variant'access);
							
						when others => invalid_section;
					end case;

				when SEC_ASSEMBLY_VARIANTS =>
					case stack.parent is
						when SEC_INIT => null; -- CS test if active variant exists
						when others => invalid_section;
					end case;

				when SEC_META =>
					case stack.parent is
						when SEC_INIT => set_meta;
						when others => invalid_section;
					end case;

				when SEC_PREFERRED_LIBRARIES =>
					case stack.parent is
						when SEC_SCHEMATIC =>
							case stack.parent (degree => 2) is
								when SEC_META	=> null; -- nothing to do
								when others		=> invalid_section;
							end case;

						when SEC_BOARD =>
							case stack.parent (degree => 2) is
								when SEC_META	=> null; -- nothing to do
								when others		=> invalid_section;
							end case;
							
						when others => invalid_section;
					end case;
					
				when SEC_RULES =>
					case stack.parent is
						when SEC_INIT => set_rules;
						when others => invalid_section;
					end case;
					
				when SEC_NETCHANGERS =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;

				when SEC_NETCHANGER =>
					case stack.parent is
						when SEC_NETCHANGERS =>

							-- insert netchanger in module
							update_element (
								container	=> generic_modules,
								position	=> module_cursor,
								process		=> insert_netchanger'access);
							
						when others => invalid_section;
					end case;
					
				when SEC_DEVICES_NON_ELECTRIC | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
					SEC_STOP_MASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
					SEC_COPPER | SEC_PCB_CONTOURS_NON_PLATED =>
					case stack.parent is
						when SEC_BOARD => null;
						when others => invalid_section;
					end case;

				when SEC_TOP | SEC_BOTTOM =>
					case stack.parent is
						when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
							SEC_STOP_MASK | SEC_KEEPOUT => null;

						when others => invalid_section;
					end case;
					
				when SEC_INIT => null; -- CS: should never happen
			end case;

-- 				exception when event:
-- 					others => 
-- 						log (text => ada.exceptions.exception_message (event), console => true);
-- 						raise;
			
		end execute_section;
		
		function set (
		-- Tests if the current line is a section header or footer. Returns true in both cases.
		-- Returns false if the current line is neither a section header or footer.
		-- If it is a header, the section name is pushed onto the sections stack.
		-- If it is a footer, the latest section name is popped from the stack.
			section_keyword	: in string; -- [NETS
			section			: in type_section) -- SEC_NETS
			return boolean is 
		begin -- set
			if f (line, 1) = section_keyword then -- section name detected in field 1
				if f (line, 2) = section_begin then -- section header detected in field 2
					stack.push (section);
					log (text => write_enter_section & to_string (section), level => log_threshold + 5);
					return true;

				elsif f (line, 2) = section_end then -- section footer detected in field 2

					-- The section name in the footer must match the name
					-- of the current section. Otherwise abort.
					if section /= stack.current then
						log_indentation_reset;
						invalid_section;
					end if;
					
					-- Now that the section ends, the data collected in temporarily
					-- variables is processed.
					execute_section;
					
					stack.pop;
					if stack.empty then
						log (text => write_top_level_reached, level => log_threshold + 5);
					else
						log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 5);
					end if;
					return true;

				else
					log (ERROR, write_missing_begin_end, console => true);
					raise constraint_error;
				end if;

			else -- neither a section header nor footer
				return false;
			end if;
		end set;

	begin -- process_line
		if set (section_net_classes, SEC_NET_CLASSES) then null;
		elsif set (section_net_class, SEC_NET_CLASS) then null;
		elsif set (section_board_layer_stack, SEC_BOARD_LAYER_STACK) then null;			
		elsif set (section_drawing_grid, SEC_DRAWING_GRID) then null;
		elsif set (section_nets, SEC_NETS) then null;
		elsif set (section_net, SEC_NET) then null;
		elsif set (section_strands, SEC_STRANDS) then null;
		elsif set (section_strand, SEC_STRAND) then null;
		elsif set (section_segments, SEC_SEGMENTS) then null;
		elsif set (section_segment, SEC_SEGMENT) then null;
		elsif set (section_labels, SEC_LABELS) then null;
		elsif set (section_label, SEC_LABEL) then null;
		elsif set (section_ports, SEC_PORTS) then null;
		elsif set (section_port, SEC_PORT) then null;				
		elsif set (section_route, SEC_ROUTE) then null;								
		elsif set (section_line, SEC_LINE) then null;								
		elsif set (section_arc, SEC_ARC) then null;
		elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
		elsif set (section_fill_zone, SEC_FILL_ZONE) then null;								
		elsif set (section_contours, SEC_CONTOURS) then null;								
		elsif set (section_via, SEC_VIA) then null;								
		elsif set (section_submodules, SEC_SUBMODULES) then null;
		elsif set (section_submodule, SEC_SUBMODULE) then null;
		elsif set (section_drawing_frames, SEC_DRAWING_FRAMES) then null;
		elsif set (section_schematic, SEC_SCHEMATIC) then null;
		elsif set (section_sheet_descriptions, SEC_SHEET_DESCRIPTIONS) then null;
		elsif set (section_sheet, SEC_SHEET) then null;
		elsif set (section_board, SEC_BOARD) then null;
		elsif set (section_devices, SEC_DEVICES) then null;
		elsif set (section_device, SEC_DEVICE) then null;
		elsif set (section_devices_non_electric, SEC_DEVICES_NON_ELECTRIC) then null;
		elsif set (section_units, SEC_UNITS) then null;
		elsif set (section_unit, SEC_UNIT) then null;
		elsif set (section_assembly_variants, SEC_ASSEMBLY_VARIANTS) then null;
		elsif set (section_assembly_variant, SEC_ASSEMBLY_VARIANT) then null;
		elsif set (section_netchangers, SEC_NETCHANGERS) then null;
		elsif set (section_netchanger, SEC_NETCHANGER) then null;
		elsif set (section_meta, SEC_META) then null;
		elsif set (section_preferred_libraries, SEC_PREFERRED_LIBRARIES) then null;
		elsif set (section_rules, SEC_RULES) then null;			
		elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;				
		elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
		elsif set (section_package, SEC_PACKAGE) then null;
		elsif set (section_texts, SEC_TEXTS) then null;
		elsif set (section_text, SEC_TEXT) then null;
		elsif set (section_silk_screen, SEC_SILK_SCREEN) then null;
		elsif set (section_top, SEC_TOP) then null;
		elsif set (section_bottom, SEC_BOTTOM) then null;
		elsif set (section_circle, SEC_CIRCLE) then null;
		elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
		elsif set (section_stencil, SEC_STENCIL) then null;
		elsif set (section_stop_mask, SEC_STOP_MASK) then null;
		elsif set (section_keepout, SEC_KEEPOUT) then null;
		elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;
		elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
		elsif set (section_copper, SEC_COPPER) then null;				
		elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
		else
			-- The line contains something else -> the payload data. 
			-- Temporarily this data is stored in corresponding variables.

			log (text => "module line --> " & to_string (line), level => log_threshold + 4);
	
			case stack.current is

				when SEC_CONTOURS =>
					case stack.parent is
						when SEC_FILL_ZONE => null;
						when SEC_CUTOUT_ZONE => null;
						when others => invalid_section;
					end case;
				
				when SEC_NET_CLASSES =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_DRAWING_GRID =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_BOARD_LAYER_STACK =>
					case stack.parent is
						when SEC_INIT => read_layer;
						when others => invalid_section;
					end case;
					
				when SEC_DEVICES =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_ASSEMBLY_VARIANTS =>
					case stack.parent is
						when SEC_INIT => set_active_assembly_variant;
						when others => invalid_section;
					end case;

				when SEC_ASSEMBLY_VARIANT =>
					case stack.parent is
						when SEC_ASSEMBLY_VARIANTS =>
							declare
								use et_devices;
								kw : string 	:= f (line, 1);
								device_name		: type_device_name; -- R1
								device			: access et_assembly_variants.type_device;
								device_cursor	: et_assembly_variants.pac_device_variants.cursor;
								
								submod_name		: et_general.pac_module_instance_name.bounded_string; -- MOT_DRV_3
								submod_var		: et_general.pac_assembly_variant_name.bounded_string; -- low_cost
								submod_cursor	: et_assembly_variants.pac_submodule_variants.cursor;
								inserted		: boolean;
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_name then -- name low_cost
									expect_field_count (line, 2);
									assembly_variant_name := et_general.to_variant (f (line, 2));

								elsif kw = keyword_description then -- description "variant without temperature sensor"
									expect_field_count (line, 2);

									assembly_variant_description := et_assembly_variants.to_unbounded_string (f (line, 2));
									
								-- A line like "device R1 not_mounted" or
								-- a line like "device R1 value 270R partcode 12345" or		
								-- a line like "device R1 value 270R partcode 12345 purpose "set temperature""
								-- tells whether a device is mounted or not.
								elsif kw = keyword_device then

									-- there must be at least 3 fields:
									expect_field_count (line, 3, warn => false);
									
									device_name := to_device_name (f (line, 2));

									-- test whether device exists
									if not exists (module_cursor, device_name) then
										log (ERROR, "device " &
												enclose_in_quotes (to_string (device_name)) &
												" does not exist !", console => true);
										raise constraint_error;
									end if;

									if f (line, 3) = keyword_not_mounted then
										-- line like "device R1 not_mounted"

										device := new et_assembly_variants.type_device'(
											mounted	=> et_assembly_variants.NO);
										
									elsif f (line, 3) = keyword_value then
										-- line like "device R1 value 270R partcode 12345"

										-- create a device with discriminant "mounted" where
										-- pointer assembly_variant_device is pointing at.
										device := new et_assembly_variants.type_device'(
											mounted	=> et_assembly_variants.YES,
											others	=> <>); -- to be assigned later
										
										-- there must be at least 6 fields:
										expect_field_count (line, 6, warn => false);

										-- read and validate value
										device.value := to_value_with_check (f (line, 4));

										-- read partcode
										if f (line, 5) = et_material.keyword_partcode then
											device.partcode := et_material.to_partcode (f (line, 6));
										else -- keyword partcode not found
											log (ERROR, "expect keyword " & enclose_in_quotes (et_material.keyword_partcode) &
													" after value !", console => true);
											raise constraint_error;
										end if;

										-- read optional purpose
										if field_count (line) > 6 then
											expect_field_count (line, 8);

											if f (line, 7) = keyword_purpose then

												-- validate purpose
												device.purpose := to_purpose (f (line, 8));

											else -- keyword purpose not found
												log (ERROR, "expect keyword " & enclose_in_quotes (keyword_purpose) &
													" after partcode !", console => true);
												raise constraint_error;
											end if;
										end if;
											
									else -- keyword value not found
										log (ERROR, "expect keyword " & enclose_in_quotes (keyword_value) &
												" or keyword " & enclose_in_quotes (keyword_not_mounted) &
												" after device name !", console => true);
										raise constraint_error;
									end if;											

									-- Insert the device in the current assembly variant:
									et_assembly_variants.pac_device_variants.insert (
										container	=> assembly_variant_devices,
										key			=> device_name, -- R1
										new_item	=> device.all,
										inserted	=> inserted,
										position	=> device_cursor);

									-- Raise error if device occurs more than once:
									if not inserted then
										log (ERROR, "device " &
												enclose_in_quotes (to_string (device_name)) &
												" already specified !", console => true);
										raise constraint_error;
									end if;

								-- a line like "submodule OSC1 variant low_cost
								-- tells which assembly variant of a submodule is used:
								elsif kw = keyword_submodule then

									-- there must be 4 fields:
									expect_field_count (line, 4);

									submod_name := et_general.to_instance_name (f (line, 2)); -- OSC1

									-- test whether submodule instance exists
									if not exists (module_cursor, submod_name) then
										log (ERROR, "submodule instance " &
												enclose_in_quotes (et_general.to_string (submod_name)) &
												" does not exist !", console => true);
										raise constraint_error;
									end if;

									-- After the instance name (like OSC1) must come the keyword "variant"
									-- followed by the variant name:
									if f (line, 3) = keyword_variant then
										submod_var := et_general.to_variant (f (line, 4));
										
										-- NOTE: A test whether the submodule does provide the variant can
										-- not be executed at this stage because the submodules have not 
										-- been read yet. This will be done after procdure 
										-- read_submodule_files has been executed. See far below.

										-- Insert the submodule in the current assembly variant:
										et_assembly_variants.pac_submodule_variants.insert (
											container	=> assembly_variant_submodules,
											key			=> submod_name, -- OSC1
											new_item	=> (variant => submod_var), -- type_submodule is a record with currently only one element
											inserted	=> inserted,
											position	=> submod_cursor);

										-- Raise error if submodule occurs more than once:
										if not inserted then
											log (ERROR, "submodule " &
												enclose_in_quotes (et_general.to_string (submod_name)) &
												" already specified !", console => true);
											raise constraint_error;
										end if;

									else
										log (ERROR, "expect keyword " & enclose_in_quotes (keyword_variant) &
												" after instance name !", console => true);
										raise constraint_error;
									end if;
									
								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;
					
				when SEC_TEXTS =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do								
						when others => invalid_section;
					end case;

				when SEC_SUBMODULES =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do								
						when others => invalid_section;
					end case;

				when SEC_DRAWING_FRAMES =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do								
						when others => invalid_section;
					end case;
					
				when SEC_META =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_PREFERRED_LIBRARIES =>
					case stack.parent is
						when SEC_SCHEMATIC =>
							case stack.parent (degree => 2) is
								when SEC_META	=> read_preferred_lib_schematic;
								when others		=> invalid_section;
							end case;

						when SEC_BOARD =>
							case stack.parent (degree => 2) is
								when SEC_META	=> read_preferred_lib_board;
								when others		=> invalid_section;
							end case;
							
						when others => invalid_section;
					end case;
					
				when SEC_RULES =>
					case stack.parent is
						when SEC_INIT => read_rules;
						when others => invalid_section;
					end case;
					
				when SEC_NET_CLASS =>
					case stack.parent is
						when SEC_NET_CLASSES =>
							declare
								use et_terminals;
								use et_drills;
								use et_packages;									
								use et_pcb;
								kw : string := f (line, 1);
							begin
								if kw = keyword_name then
									expect_field_count (line, 2);
									net_class_name := to_net_class_name (f (line,2));

								-- CS: In the following: set a corresponding parameter-found-flag
								elsif kw = keyword_description then
									expect_field_count (line, 2);
									net_class.description := to_net_class_description (f (line,2));
									
								elsif kw = keyword_clearance then
									expect_field_count (line, 2);
									net_class.clearance := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_track_clearance (net_class.clearance);
									
								elsif kw = keyword_track_width_min then
									expect_field_count (line, 2);
									net_class.track_width_min := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_track_width (net_class.track_width_min);
									
								elsif kw = keyword_via_drill_min then
									expect_field_count (line, 2);
									net_class.via_drill_min := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_drill_size (net_class.via_drill_min);
									
								elsif kw = keyword_via_restring_min then
									expect_field_count (line, 2);
									net_class.via_restring_min := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_restring_width (net_class.via_restring_min);
									
								elsif kw = keyword_micro_via_drill_min then
									expect_field_count (line, 2);
									net_class.micro_via_drill_min := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_drill_size (net_class.micro_via_drill_min);
									
								elsif kw = keyword_micro_via_restring_min then
									expect_field_count (line, 2);
									net_class.micro_via_restring_min := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line,2));
									validate_restring_width (net_class.micro_via_restring_min);
								else
									invalid_keyword (kw);
								end if;
							end;

						when others => invalid_section;
					end case;

				when SEC_STRAND =>
					case stack.parent is
						when SEC_STRANDS =>
							declare
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_position then -- position sheet 1 x 1.000 y 5.555
									expect_field_count (line, 7);

									-- extract strand position starting at field 2
									strand.position := to_position (line, 2);
								else
									invalid_keyword (kw);
								end if;
							end;
						
						when others => invalid_section;
					end case;
					
				when SEC_STRANDS =>
					case stack.parent is
						when SEC_NET => null; -- nothing to do
						when others => invalid_section;
					end case;
				
				when SEC_ROUTE =>
					case stack.parent is
						when SEC_NET => null; -- nothing to do
						when others => invalid_section;
					end case;
				
				when SEC_NET =>
					case stack.parent is
						when SEC_NETS =>
							declare
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_name then
									expect_field_count (line, 2);
									net_name := to_net_name (f (line,2));
									
								elsif kw = keyword_class then
									-- CS: imported kicad projects lack the class name sometimes.
									-- For this reason we do not abort in such cases but issue a warning.
									-- If abort is a must, the next two statements are required. 
									-- The "if" construct must be in comments instead.
									-- It is perhaps more reasonable to care for this flaw in et_kicad_pcb package.
									
									-- expect_field_count (line, 2);
									-- net.class := et_pcb.to_net_class_name (f (line,2));
									
									if field_count (line) = 2 then
										net.class := et_pcb.to_net_class_name (f (line,2));
									else
										net.class := et_pcb.net_class_name_default;
										log (text => message_warning & affected_line (line) & "No net class specified ! Assume default class !");
									end if;
								elsif kw = keyword_scope then
									expect_field_count (line, 2);
									net.scope := et_netlists.to_net_scope (f (line,2));
									
								else
									invalid_keyword (kw);
								end if;
							end;
									
						when others => invalid_section;
					end case;

				when SEC_NETS =>
					case stack.parent is
						when SEC_INIT => null;
						when others => invalid_section;
					end case;
					
				when SEC_SEGMENT =>
					case stack.parent is
						when SEC_SEGMENTS =>
							declare
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_start then -- "start x 3 y 4"
									expect_field_count (line, 5);

									-- extract start position starting at field 2
									net_segment.start_point := to_position (line, from => 2);
									
								elsif kw = keyword_end then -- "end x 6 y 4"
									expect_field_count (line, 5);

									-- extract end position starting at field 2
									net_segment.end_point := to_position (line, from => 2);

								elsif kw = keyword_junction then -- "junction start/end"
									expect_field_count (line, 2);
									set_junction (f (line, 2));
								else
									invalid_keyword (kw);
								end if;
							end;
									
						when others => invalid_section;
					end case;

				when SEC_SEGMENTS =>
					case stack.parent is
						when SEC_STRAND => null; -- nothing to do
						when others => invalid_section;
					end case;
					
				when SEC_LABELS =>
					case stack.parent is
						when SEC_SEGMENT => null; -- nothing to do
						when others => invalid_section;
					end case;
					
				when SEC_PORTS =>
					case stack.parent is 
						when SEC_SEGMENT =>
							-- read port parameters
							-- NOTE: A device, submodule or netchanger port is defined by a
							-- single line.
							-- Upon reading the line like "device/submodule/netchanger x port 1" 
							-- the port is appended to the corresponding port collection 
							-- immediately when the line is read. See main code of process_line.
							declare
								use et_symbols;
								kw : string := f (line, 1);
							begin
								if kw = keyword_device then -- device R1 port 1
									expect_field_count (line, 4);

									net_device_port.device_name := to_device_name (f (line, 2)); -- IC3

									if f (line, 3) = keyword_port then -- port
										net_device_port.port_name := to_port_name (f (line, 4)); -- CE

										-- Insert port in port collection of device ports. First make sure it is
										-- not already in the net segment.
										if et_schematic.pac_device_ports.contains (net_device_ports, net_device_port) then
											log (ERROR, "device " & to_string (net_device_port.device_name) &
												" port " & to_string (net_device_port.port_name) & 
												" already in net segment !", console => true);
											raise constraint_error;
										end if;

										et_schematic.pac_device_ports.insert (net_device_ports, net_device_port); 

									else
										invalid_keyword (f (line, 3));
									end if;

								elsif kw = keyword_submodule then -- submodule motor_driver port mot_on_off
									expect_field_count (line, 4);
									
									net_submodule_port.module_name := et_general.to_instance_name (f (line, 2)); -- motor_driver

									if f (line, 3) = keyword_port then -- port
										net_submodule_port.port_name := to_net_name (f (line, 4)); -- A

										-- Insert submodule port in collection of submodule ports. First make sure it is
										-- not already in the net segment.
										if et_schematic.pac_submodule_ports.contains (net_submodule_ports, net_submodule_port) then
											log (ERROR, "submodule " & to_string (net_submodule_port.module_name) &
												" port " & et_general.to_string (net_submodule_port.port_name) & 
												" already in net segment !", console => true);
											raise constraint_error;
										end if;
										
										et_schematic.pac_submodule_ports.insert (net_submodule_ports, net_submodule_port);

										-- clean up for next submodule port
										net_submodule_port := (others => <>);

									else
										invalid_keyword (f (line, 3));
									end if;

								elsif kw = keyword_netchanger then -- netchanger 1 port master/slave
									expect_field_count (line, 4);
									
									net_netchanger_port.index := et_submodules.to_netchanger_id (f (line, 2)); -- 1

									if f (line, 3) = keyword_port then -- port
										net_netchanger_port.port := et_submodules.to_port_name (f (line, 4)); -- MASTER, SLAVE

										-- Insert netchanger port in collection of netchanger ports. First make sure it is
										-- not already in the net segment.
										if et_netlists.pac_netchanger_ports.contains (net_netchanger_ports, net_netchanger_port) then
											log (ERROR, "netchanger" & et_submodules.to_string (net_netchanger_port.index) &
												et_submodules.to_string (net_netchanger_port.port) & " port" & 
												" already in net segment !", console => true);
											raise constraint_error;
										end if;
										
										et_netlists.pac_netchanger_ports.insert (net_netchanger_ports, net_netchanger_port);

										-- clean up for next netchanger port
										net_netchanger_port := (others => <>);

									else
										invalid_keyword (f (line, 3));
									end if;
									
								else
									invalid_keyword (kw);
								end if;
							end;

						when SEC_SUBMODULE => null; -- nothing to do
							
						when others => invalid_section;
					end case;

				when SEC_LABEL =>
					case stack.parent is
						when SEC_LABELS =>
							declare
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_position then -- position x 148.59 y 104.59
									expect_field_count (line, 5);

									-- extract label position starting at field 2 of line
									net_label.position := to_position (line, 2);
									
								elsif kw = keyword_rotation then -- rotation 0.0
									expect_field_count (line, 2);
									net_label_rotation := pac_geometry_sch.to_rotation (f (line, 2));

								elsif kw = et_text.keyword_size then -- size 1.3
									expect_field_count (line, 2);
									net_label.size := pac_geometry_sch.to_distance (f (line, 2));

-- 									elsif kw = keyword_style then -- style normal
-- 										expect_field_count (line, 2);
-- 										net_label.style := et_symbols.to_text_style (f (line, 2));

								elsif kw = et_text.keyword_line_width then -- line_width 0.1
									expect_field_count (line, 2);
									net_label.width := pac_geometry_sch.to_distance (f (line, 2));

								elsif kw = keyword_appearance then -- appearance tag/simple
									expect_field_count (line, 2);
									net_label_appearance := et_schematic.to_appearance (f (line, 2));

								elsif kw = et_schematic.keyword_direction then -- direction input/output
									expect_field_count (line, 2);
									net_label_direction := et_schematic.to_direction (f (line, 2));
									
								else
									invalid_keyword (kw);
								end if;
							end;

						when others => invalid_section;
					end case;
				
				when SEC_LINE =>
					case stack.parent is

						when SEC_CONTOURS => read_board_line (line); -- of a cutout or fill zone
							
						when SEC_ROUTE =>
							if not read_board_line (line) then
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layer then -- layer 2
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										validate_signal_layer;

									elsif kw = et_pcb_rw.keyword_width then -- width 0.5
										expect_field_count (line, 2);
										board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when SEC_TOP | SEC_BOTTOM => 
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
									SEC_STENCIL | SEC_STOP_MASK =>

									if not read_board_line (line) then
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = et_pcb_rw.keyword_width then -- width 0.5
												expect_field_count (line, 2);
												board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;
									end if;
									
								when SEC_KEEPOUT => read_board_line (line);
									
								when others => invalid_section;
							end case;
							
						when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							
							if not read_board_line (line) then
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);
										signal_layers := to_layers (line, check_layers);
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when SEC_COPPER =>
							if not read_board_line (line) then
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = et_pcb_rw.keyword_width then -- width 0.5
										expect_field_count (line, 2);
										board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));

									elsif kw = keyword_layer then -- layer 1
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										validate_signal_layer;

									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when SEC_PCB_CONTOURS_NON_PLATED => 

							if not read_board_line (line) then
								declare
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_locked then -- locked no
										expect_field_count (line, 2);
										board_lock_status := et_pcb.to_lock_status (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when others => invalid_section;
					end case;

				when SEC_ARC =>
					case stack.parent is

						when SEC_CONTOURS => read_board_arc (line);
						
						when SEC_ROUTE =>
							if not read_board_arc (line) then
								declare
									kw : string := f (line, 1);
									use et_terminals.pac_shapes;
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layer then -- layer 2
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										validate_signal_layer;
										
									elsif kw = et_pcb_rw.keyword_width then -- width 0.5
										expect_field_count (line, 2);
										board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
								
						when SEC_TOP | SEC_BOTTOM => 
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
									SEC_STENCIL | SEC_STOP_MASK =>

									if not read_board_arc (line) then
									
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = et_pcb_rw.keyword_width then -- width 0.5
												expect_field_count (line, 2);
												board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									end if;
									
								when SEC_KEEPOUT => read_board_arc (line);
									
								when others => invalid_section;
							end case;

						when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							if not read_board_arc (line) then
							
								declare
									kw : string := f (line, 1);
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);

										signal_layers := to_layers (line, check_layers);

									else
										invalid_keyword (kw);
									end if;
								end;

							end if;
							
						when SEC_COPPER =>
							if not read_board_arc (line) then
								declare
									kw : string := f (line, 1);
									use et_terminals.pac_shapes;
									use et_pcb_stack;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = et_pcb_rw.keyword_width then -- width 0.5
										expect_field_count (line, 2);
										board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));

									elsif kw = keyword_layer then -- layer 1
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										validate_signal_layer;
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when SEC_PCB_CONTOURS_NON_PLATED =>
							if not read_board_arc (line) then
								declare
									kw : string := f (line, 1);
									use et_terminals.pac_shapes;
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_locked then -- locked no
										expect_field_count (line, 2);
										board_lock_status := et_pcb.to_lock_status (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when others => invalid_section;
					end case;

				when SEC_CIRCLE =>
					case stack.parent is

						when SEC_CONTOURS => read_board_circle (line);
						
						when SEC_TOP | SEC_BOTTOM => 
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
									SEC_STENCIL | SEC_STOP_MASK =>

									if not read_board_circle (line) then
									
										declare
											use et_packages;
											use et_terminals.pac_shapes;
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = et_pcb_rw.keyword_width then -- circumfence line width 0.5
												expect_field_count (line, 2);
												board_line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));

											elsif kw = keyword_filled then -- filled yes/no
												expect_field_count (line, 2);													
												board_filled := to_filled (f (line, 2));

											elsif kw = keyword_fill_style then -- fill_style solid/hatched
												expect_field_count (line, 2);													
												board_fill_style := to_fill_style (f (line, 2));

											elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
												expect_field_count (line, 2);													
												board_hatching.line_width := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));

											elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
												expect_field_count (line, 2);													
												board_hatching.spacing := et_pcb_coordinates.pac_geometry_brd.to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									end if;
									
								when SEC_KEEPOUT =>
									if not read_board_circle (line) then
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_filled then -- filled yes/no
												expect_field_count (line, 2);													
												board_filled := to_filled (f (line, 2));
											else
												invalid_keyword (kw);
											end if;
										end;
									end if;
									
								when others => invalid_section;

							end case;

						when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							if not read_board_circle (line) then

								declare
									use et_pcb_stack;
									use et_packages;
									use et_terminals.pac_shapes;
									use et_pcb_coordinates.pac_geometry_brd;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);													
										board_filled := to_filled (f (line, 2));

									elsif kw = keyword_layers then -- layers 1 14 3

										-- there must be at least two fields:
										expect_field_count (line => line, count_expected => 2, warn => false);

										signal_layers := to_layers (line, check_layers);
										
									else
										invalid_keyword (kw);
									end if;
								end;

							end if;
							
						when SEC_COPPER =>
							if not read_board_circle (line) then
								declare
									use et_packages;
									use et_terminals.pac_shapes;
									use et_pcb_stack;
									use et_pcb_coordinates.pac_geometry_brd;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = et_pcb_rw.keyword_width then -- width 0.5
										expect_field_count (line, 2);
										board_line_width := to_distance (f (line, 2));

									elsif kw = keyword_filled then -- filled yes/no
										expect_field_count (line, 2);													
										board_filled := to_filled (f (line, 2));

									elsif kw = keyword_fill_style then -- fill_style solid/hatched
										expect_field_count (line, 2);													
										board_fill_style := to_fill_style (f (line, 2));

									elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
										expect_field_count (line, 2);													
										board_hatching_copper.line_width := to_distance (f (line, 2));

									elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
										expect_field_count (line, 2);													
										board_hatching_copper.spacing := to_distance (f (line, 2));

									elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
										expect_field_count (line, 2);													
										board_hatching_copper.border_width := to_distance (f (line, 2));
										
									elsif kw = keyword_layer then -- layer 1
										expect_field_count (line, 2);
										signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
										validate_signal_layer;

									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when SEC_PCB_CONTOURS_NON_PLATED =>
							if not read_board_circle (line) then
								declare
									use et_pcb_coordinates.pac_geometry_brd;
									use et_terminals.pac_shapes;
									kw : string := f (line, 1);
								begin
									-- CS: In the following: set a corresponding parameter-found-flag
									if kw = keyword_locked then -- locked no
										expect_field_count (line, 2);
										board_lock_status := et_pcb.to_lock_status (f (line, 2));
										
									else
										invalid_keyword (kw);
									end if;
								end;
							end if;
							
						when others => invalid_section;
					end case;

				when SEC_CUTOUT_ZONE =>
					case stack.parent is
						when SEC_ROUTE =>
							declare
								use et_packages;
								use et_pcb_coordinates.pac_geometry_brd;
								use et_pcb_stack;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
									expect_field_count (line, 2);
									board_easing.style := to_corner_easing (f (line, 2));

								elsif kw = keyword_easing_radius then -- easing_radius 0.3
									expect_field_count (line, 2);
									board_easing.radius := to_distance (f (line, 2));

								elsif kw = keyword_layer then -- layer 2
									expect_field_count (line, 2);
									signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer;

								else
									invalid_keyword (kw);
								end if;
							end;

						when SEC_TOP | SEC_BOTTOM => 
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
									SEC_STENCIL | SEC_STOP_MASK =>
									declare
										use et_packages;
										use et_terminals.pac_shapes;
										use et_pcb_coordinates.pac_geometry_brd;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
											expect_field_count (line, 2);													
											board_easing.style := to_corner_easing (f (line, 2));

										elsif kw = keyword_easing_radius then -- easing_radius 0.4
											expect_field_count (line, 2);													
											board_easing.radius := to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;

								when SEC_KEEPOUT =>
									-- no parameters allowed here
									declare
										kw : string := f (line, 1);
									begin
										invalid_keyword (kw);
									end;
									
								when others => invalid_section;
							end case;

						when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							declare
								use et_pcb_stack;
								use et_packages;
								use et_terminals.pac_shapes;
								use et_pcb_coordinates.pac_geometry_brd;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_layers then -- layers 1 14 3

									-- there must be at least two fields:
									expect_field_count (line => line, count_expected => 2, warn => false);
									signal_layers := to_layers (line, check_layers);

								else
									invalid_keyword (kw);
								end if;
							end;

						when SEC_COPPER => -- non electrical
							declare
								use et_packages;
								use et_terminals.pac_shapes;									
								use et_pcb_stack;
								use et_pcb_coordinates.pac_geometry_brd;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
									expect_field_count (line, 2);													
									board_easing.style := to_corner_easing (f (line, 2));

								elsif kw = keyword_easing_radius then -- easing_radius 0.4
									expect_field_count (line, 2);													
									board_easing.radius := to_distance (f (line, 2));
									
								elsif kw = keyword_layer then -- layer 1
									expect_field_count (line, 2);
									signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer;

								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;
					
				when SEC_FILL_ZONE =>
					case stack.parent is
						when SEC_ROUTE => -- connected with a net
							declare
								use et_packages;
								use et_pcb_coordinates.pac_geometry_brd;
								use et_pcb;
								use et_pcb_stack;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = et_pcb.keyword_priority then -- priority 2
									expect_field_count (line, 2);
									polygon_priority := et_pcb.to_polygon_priority (f (line, 2));

								elsif kw = keyword_isolation then -- isolation 0.5
									expect_field_count (line, 2);
									polygon_isolation := to_distance (f (line, 2));
									
								elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
									expect_field_count (line, 2);
									board_easing.style := to_corner_easing (f (line, 2));

								elsif kw = keyword_easing_radius then -- easing_radius 0.3
									expect_field_count (line, 2);
									board_easing.radius := to_distance (f (line, 2));

								elsif kw = keyword_fill_style then -- fill_style solid,hatched
									expect_field_count (line, 2);
									board_fill_style := to_fill_style (f (line, 2));

								elsif kw = keyword_hatching_line_width then -- hatching_line_width 1
									expect_field_count (line, 2);
									board_hatching_copper.line_width := to_distance (f (line, 2));

								elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 1
									expect_field_count (line, 2);
									board_hatching_copper.spacing := to_distance (f (line, 2));

								elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
									expect_field_count (line, 2);
									board_hatching_copper.border_width := to_distance (f (line, 2));
									
								elsif kw = keyword_layer then -- layer 2
									expect_field_count (line, 2);
									signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer;
									
								elsif kw = keyword_min_width then -- min_width 0.3
									expect_field_count (line, 2);
									polygon_width_min := to_distance (f (line, 2));

								elsif kw = keyword_pad_technology then -- pad_technology smt_only/tht_only/smt_and_tht
									expect_field_count (line, 2);
									et_pcb_rw.thermal.technology := to_pad_technology (f (line, 2));

								elsif kw = keyword_pad_connection then -- pad_connection thermal/solid
									expect_field_count (line, 2);
									polygon_pad_connection := to_pad_connection (f (line, 2));
									
								elsif kw = keyword_thermal_width then -- thermal_width 0.3
									expect_field_count (line, 2);
									et_pcb_rw.thermal.width := to_distance (f (line, 2));

								elsif kw = keyword_thermal_gap then -- thermal_gap 0.7
									expect_field_count (line, 2);
									et_pcb_rw.thermal.gap := to_distance (f (line, 2));

								else
									invalid_keyword (kw);
								end if;
							end;

						when SEC_TOP | SEC_BOTTOM => 
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION |
									SEC_STENCIL | SEC_STOP_MASK =>
									declare
										use et_packages;
										use et_terminals.pac_shapes;
										use et_pcb_coordinates.pac_geometry_brd;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_fill_style then -- fill_style solid/hatched
											expect_field_count (line, 2);													
											board_fill_style := to_fill_style (f (line, 2));

										elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
											expect_field_count (line, 2);													
											board_easing.style := to_corner_easing (f (line, 2));

										elsif kw = keyword_easing_radius then -- easing_radius 0.4
											expect_field_count (line, 2);													
											board_easing.radius := to_distance (f (line, 2));
											
										elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
											expect_field_count (line, 2);													
											board_hatching.line_width := to_distance (f (line, 2));

										elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
											expect_field_count (line, 2);													
											board_hatching.spacing := to_distance (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;

								when SEC_KEEPOUT =>
									declare
										use et_terminals.pac_shapes;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_filled then -- filled yes/no
											expect_field_count (line, 2);													
											board_filled := to_filled (f (line, 2));

										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
							end case;

						when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							declare
								use et_pcb_stack;
								use et_packages;
								use et_terminals.pac_shapes;
								use et_pcb_coordinates.pac_geometry_brd;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_filled then -- filled yes/no
									expect_field_count (line, 2);													
									board_filled := to_filled (f (line, 2));

								elsif kw = keyword_layers then -- layers 1 14 3

									-- there must be at least two fields:
									expect_field_count (line => line, count_expected => 2, warn => false);
									signal_layers := to_layers (line, check_layers);

								else
									invalid_keyword (kw);
								end if;
							end;

						when SEC_COPPER => -- non electrical
							declare
								use et_packages;
								use et_terminals.pac_shapes;									
								use et_pcb_stack;
								use et_pcb_coordinates.pac_geometry_brd;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_fill_style then -- fill_style solid/hatched
									expect_field_count (line, 2);													
									board_fill_style := to_fill_style (f (line, 2));

								elsif kw = keyword_corner_easing then -- corner_easing none/chamfer/fillet
									expect_field_count (line, 2);													
									board_easing.style := to_corner_easing (f (line, 2));

								elsif kw = keyword_easing_radius then -- easing_radius 0.4
									expect_field_count (line, 2);													
									board_easing.radius := to_distance (f (line, 2));
									
								elsif kw = keyword_hatching_line_width then -- hatching_line_width 0.3
									expect_field_count (line, 2);													
									board_hatching_copper.line_width := to_distance (f (line, 2));

								elsif kw = keyword_hatching_line_spacing then -- hatching_line_spacing 0.3
									expect_field_count (line, 2);													
									board_hatching_copper.spacing := to_distance (f (line, 2));

								elsif kw = keyword_hatching_border_width then -- hatching_border_width 1
									expect_field_count (line, 2);													
									board_hatching_copper.border_width := to_distance (f (line, 2));
									
								elsif kw = keyword_min_width then -- min_width 0.5
									expect_field_count (line, 2);
									polygon_width_min := to_distance (f (line, 2));
									
								elsif kw = keyword_layer then -- layer 1
									expect_field_count (line, 2);
									signal_layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer;
									
								elsif kw = et_pcb.keyword_priority then -- priority 2
									expect_field_count (line, 2);
									polygon_priority := et_pcb.to_polygon_priority (f (line, 2));

								elsif kw = keyword_isolation then -- isolation 0.5
									expect_field_count (line, 2);
									polygon_isolation := to_distance (f (line, 2));
									
								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;

				when SEC_VIA =>
					case stack.parent is
						when SEC_ROUTE =>
							declare
								use et_pcb_coordinates.pac_geometry_brd;
								use et_pcb;
								use et_terminals;
								use et_packages;
								use et_pcb_stack;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_position then -- position x 22.3 y 23.3
									expect_field_count (line, 5);

									-- extract the position starting at field 2 of line
									route_via.position := to_position (line, 2);

								elsif kw = keyword_diameter then -- diameter 0.35
									expect_field_count (line, 2);
									route_via.diameter := to_distance (f (line, 2));

								elsif kw = keyword_restring_outer_layers then -- restring_outer_layers 0.3
									expect_field_count (line, 2);
									route_via.restring_outer := to_distance (f (line, 2));

								elsif kw = keyword_restring_inner_layers then -- restring_inner_layers 0.34
									expect_field_count (line, 2);
									route_via.restring_inner := to_distance (f (line, 2));

								elsif kw = keyword_layer_start then -- layer_start 1
									expect_field_count (line, 2);
									route_via.layer_start := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer (route_via.layer_start);
									
								elsif kw = keyword_layer_end then -- layer_end 15
									expect_field_count (line, 2);
									route_via.layer_end := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer (route_via.layer_end);
									
								else
									invalid_keyword (kw);
								end if;
							end;

						when others => invalid_section;
					end case;
				
				when SEC_SUBMODULE =>
					case stack.parent is
						when SEC_SUBMODULES =>
							declare
								use et_submodules;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_file then -- file $ET_TEMPLATES/motor_driver.mod
									expect_field_count (line, 2);
									submodule.file := et_submodules.to_submodule_path (f (line, 2));

								elsif kw = keyword_name then -- name stepper_driver
									expect_field_count (line, 2);
									submodule_name := to_instance_name (f (line, 2));

								elsif kw = keyword_position then -- position sheet 3 x 130 y 210
									expect_field_count (line, 7);

									-- extract position of submodule starting at field 2
									submodule.position := to_position (line, 2);

								elsif kw = et_submodules.keyword_size then -- size x 30 y 30
									expect_field_count (line, 5);

									-- extract size of submodule starting at field 2
									submodule.size := to_size (line, 2);

								elsif kw = keyword_position_in_board then -- position_in_board x 23 y 0.2 rotation 90.0
									expect_field_count (line, 7);

									-- extract position of submodule starting at field 2
									submodule.position_in_board := to_position (line, 2);

								elsif kw = keyword_view_mode then -- view_mode origin/instance
									expect_field_count (line, 2);
									submodule.view_mode := et_submodules.to_view_mode (f (line, 2));

								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;

				when SEC_PORT =>
					case stack.parent is
						when SEC_PORTS =>
							case stack.parent (degree => 2) is
								when SEC_SUBMODULE =>
									declare
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_name then -- name clk_out
											expect_field_count (line, 2);
											submodule_port_name := to_net_name (f (line, 2));

										elsif kw = keyword_position then -- position x 0 y 10
											expect_field_count (line, 5);

											-- extract port position starting at field 2
											submodule_port.position := to_position (line, 2);

										elsif kw = et_submodules.keyword_direction then -- direction master/slave
											expect_field_count (line, 2);

											submodule_port.direction := et_submodules.to_port_name (f (line, 2));
										else
											invalid_keyword (kw);
										end if;
									end;

								when others => invalid_section;
							end case;

						when others => invalid_section;
					end case;
					
				when SEC_SCHEMATIC =>
					case stack.parent is
						when SEC_DRAWING_FRAMES => read_frame_template_schematic;
						when SEC_DRAWING_GRID => read_drawing_grid_schematic;
						when SEC_META => read_meta_schematic;
						when others => invalid_section;
					end case;

				when SEC_BOARD =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when SEC_DRAWING_FRAMES => read_frame_template_board;
						when SEC_DRAWING_GRID => read_drawing_grid_board;
						when SEC_META => read_meta_board;
						when others => invalid_section;
					end case;

				when SEC_SHEET_DESCRIPTIONS =>
					case stack.parent is
						when SEC_SCHEMATIC => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_SHEET =>
					case stack.parent is
						when SEC_SHEET_DESCRIPTIONS => read_sheet_description;
						when others => invalid_section;
					end case;
					
				when SEC_TEXT =>
					case stack.parent is
						when SEC_TEXTS => -- in schematic
							read_schematic_text;

						when SEC_PCB_CONTOURS_NON_PLATED => -- in board
							read_board_text_contours;
							
						when SEC_TOP | SEC_BOTTOM => -- in board
							read_board_text_non_conductor;

						when SEC_COPPER | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
							read_board_text_conductor;
							
						when others => invalid_section;
					end case;

				when SEC_DEVICE =>
					case stack.parent is
						when SEC_DEVICES => read_device;
						when SEC_DEVICES_NON_ELECTRIC => read_device_non_electric;
						when others => invalid_section;
					end case;

				when SEC_PACKAGE =>
					case stack.parent is
						when SEC_DEVICE =>
							declare
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_position then -- position x 163.500 y 92.500 rotation 0.00 face top
									expect_field_count (line, 9);

									-- extract package position starting at field 2
									device_position := to_position (line, 2);

								elsif kw = keyword_flipped then -- flipped no/yes
									expect_field_count (line, 2);

									device_flipped := et_pcb.to_flipped (f (line, 2));
								else
									invalid_keyword (kw);
								end if;
							end;

						when others => invalid_section;
					end case;

				when SEC_PLACEHOLDER =>
					case stack.parent is
						when SEC_PLACEHOLDERS =>
							case stack.parent (degree => 2) is
								when SEC_DEVICE | SEC_PACKAGE => -- in layout
									declare
										use et_packages;
										use et_pcb_stack;
										use et_pcb_coordinates.pac_geometry_brd;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_meaning then -- meaning reference, value, ...
											expect_field_count (line, 2);
											device_text_placeholder.meaning := to_text_meaning (f (line, 2));
											
										elsif kw = keyword_layer then -- layer silk_screen/assembly_documentation
											expect_field_count (line, 2);
											device_text_placeholder_layer := to_layer (f (line, 2));
											
										elsif kw = keyword_position then -- position x 0.000 y 5.555 rotation 0.00 face top
											expect_field_count (line, 9);

											-- extract position of placeholder starting at field 2
											device_text_placeholder_position := to_position (line, 2);

										elsif kw = et_text.keyword_size then -- size 5
											expect_field_count (line, 2);
											device_text_placeholder.size := to_distance (f (line, 2));

										elsif kw = et_text.keyword_line_width then -- line_width 0.15
											expect_field_count (line, 2);

											device_text_placeholder.line_width := to_distance (f (line, 2));

										elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
											expect_field_count (line, 5);

											-- extract alignment of placeholder starting at field 2
											device_text_placeholder.alignment := et_text.to_alignment (line, 2);
											
										else
											invalid_keyword (kw);
										end if;
									end;

								when SEC_UNIT =>
									declare
										use et_coordinates.pac_geometry_sch;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_meaning then -- meaning reference, value or purpose
											expect_field_count (line, 2);
											unit_placeholder_meaning := et_symbols.to_meaning (f (line, 2));
											
										elsif kw = keyword_position then -- position x 0.000 y 5.555
											expect_field_count (line, 5);

											-- extract position of placeholder starting at field 2
											unit_placeholder_position := to_position (line, 2);

										elsif kw = et_text.keyword_size then -- size 3.0
											expect_field_count (line, 2);
											unit_placeholder.size := to_distance (f (line, 2));

										elsif kw = keyword_rotation then -- rotation 90.0
											expect_field_count (line, 2);

											unit_placeholder.rotation := et_symbols.pac_text.to_rotation_doc (f (line, 2));

-- 											elsif kw = keyword_style then -- stlye italic
-- 												expect_field_count (line, 2);
-- 
-- 												unit_placeholder.style := et_symbols.to_text_style (f (line, 2));

										elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
											expect_field_count (line, 5);

											-- extract alignment of placeholder starting at field 2
											unit_placeholder.alignment := et_text.to_alignment (line, 2);
											
										else
											invalid_keyword (kw);
										end if;
									end;

								when others => invalid_section;
							end case;

						when SEC_TOP | SEC_BOTTOM =>
							case stack.parent (degree => 2) is
								when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STOP_MASK => -- CS SEC_KEEPOUT
									declare
										use et_pcb_coordinates.pac_geometry_brd;
										kw : string := f (line, 1);
									begin
										-- CS: In the following: set a corresponding parameter-found-flag
										if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
											expect_field_count (line, 7);

											-- extract position of note starting at field 2
											board_text_placeholder.position := to_position (line, 2);

										elsif kw = et_text.keyword_size then -- size 1.000
											expect_field_count (line, 2);
											board_text_placeholder.size := to_distance (f (line, 2));

										elsif kw = et_text.keyword_line_width then -- line_width 0.1
											expect_field_count (line, 2);
											board_text_placeholder.line_width := to_distance (f (line, 2));

										elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
											expect_field_count (line, 5);

											-- extract alignment starting at field 2
											board_text_placeholder.alignment := et_text.to_alignment (line, 2);
											
										elsif kw = keyword_meaning then -- meaning project_name
											expect_field_count (line, 2);
											board_text_placeholder.meaning := et_pcb.to_meaning (f (line, 2));
											
										else
											invalid_keyword (kw);
										end if;
									end;
									
								when others => invalid_section;
							end case;

						when SEC_COPPER =>
							declare
								use et_pcb_coordinates.pac_geometry_brd;
								use et_pcb_stack;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
									expect_field_count (line, 7);

									-- extract position of note starting at field 2
									board_text_copper_placeholder.position := to_position (line, 2);

								elsif kw = et_text.keyword_size then -- size 1.000
									expect_field_count (line, 2);
									board_text_copper_placeholder.size := to_distance (f (line, 2));

								elsif kw = et_text.keyword_line_width then -- line_width 0.1
									expect_field_count (line, 2);
									board_text_copper_placeholder.line_width := to_distance (f (line, 2));

								elsif kw = et_text.keyword_alignment then -- alignment horizontal center vertical center
									expect_field_count (line, 5);

									-- extract alignment starting at field 2
									board_text_copper_placeholder.alignment := et_text.to_alignment (line, 2);
									
								elsif kw = keyword_meaning then -- meaning revision/project_name/...
									expect_field_count (line, 2);
									board_text_copper_placeholder.meaning := et_pcb.to_meaning (f (line, 2));

								elsif kw = keyword_layer then -- layer 15
									expect_field_count (line, 2);
									board_text_copper_placeholder.layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer (board_text_copper_placeholder.layer);
									
								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;

				when SEC_PLACEHOLDERS =>
					case stack.parent is
						when SEC_PACKAGE => null;

						when SEC_DEVICE =>
							case stack.parent (degree => 2) is
								when SEC_DEVICES_NON_ELECTRIC => null;

								when others => invalid_section;
							end case;
						
						when SEC_UNIT => null;
						when others => invalid_section;
					end case;

				when SEC_UNIT =>
					case stack.parent is
						when SEC_UNITS =>
							declare
								use et_devices;
								kw : string := f (line, 1);
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_name then -- name 1, GPIO_BANK_1, ...
									expect_field_count (line, 2);
									device_unit_name := to_unit_name (f (line, 2));
									
								elsif kw = keyword_position then -- position sheet 1 x 1.000 y 5.555
									expect_field_count (line, 7);

									-- extract position of unit starting at field 2
									device_unit_position := to_position (line, 2);

								elsif kw = keyword_rotation then -- rotation 180.0
									expect_field_count (line, 2);
									--device_unit_rotation := geometry.to_rotation (f (line, 2));
									set (device_unit_position, pac_geometry_sch.to_rotation (f (line, 2)));

								elsif kw = keyword_mirrored then -- mirrored no/x_axis/y_axis
									expect_field_count (line, 2);
									device_unit_mirror := et_schematic.to_mirror_style (f (line, 2));

								else
									invalid_keyword (kw);
								end if;
							end;
							
						when others => invalid_section;
					end case;

				when SEC_UNITS =>
					case stack.parent is
						when SEC_DEVICE => null;
						when others => invalid_section;
					end case;

				when SEC_NETCHANGERS =>
					case stack.parent is
						when SEC_INIT => null; -- nothing to do
						when others => invalid_section;
					end case;

				when SEC_NETCHANGER =>
					case stack.parent is
						when SEC_NETCHANGERS =>
							declare
								kw : string := f (line, 1);
								use et_pcb_stack;
							begin
								-- CS: In the following: set a corresponding parameter-found-flag
								if kw = keyword_name then -- name 1, 2, 304, ...
									expect_field_count (line, 2);
									netchanger_id := et_submodules.to_netchanger_id (f (line, 2));
									
								elsif kw = keyword_position_in_schematic then -- position_in_schematic sheet 1 x 1.000 y 5.555
									expect_field_count (line, 7);

									-- extract position (in schematic) starting at field 2
									netchanger.position_sch := to_position (line, 2);

								elsif kw = keyword_rotation_in_schematic then -- rotation_in_schematic 180.0
									expect_field_count (line, 2);
									set (netchanger.position_sch, pac_geometry_sch.to_rotation (f (line, 2)));

								elsif kw = keyword_position_in_board then -- position_in_board x 55.000 y 7.555
									expect_field_count (line, 5);

									-- extract position (in board) starting at field 2
									netchanger.position_brd := to_position (line, 2);

								elsif kw = keyword_layer then -- layer 3 (signal layer in board)
									expect_field_count (line, 2);
									netchanger.layer := et_pcb_stack.to_signal_layer (f (line, 2));
									validate_signal_layer (netchanger.layer);
									
								else
									invalid_keyword (kw);
								end if;
							end;

						when others => invalid_section;
					end case;
					
				when SEC_DEVICES_NON_ELECTRIC | SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
					SEC_STOP_MASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
					SEC_COPPER | SEC_PCB_CONTOURS_NON_PLATED =>
					case stack.parent is
						when SEC_BOARD => null;
						when others => invalid_section;
					end case;

				when SEC_TOP | SEC_BOTTOM =>
					case stack.parent is
						when SEC_SILK_SCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
							SEC_STOP_MASK | SEC_KEEPOUT => null;

						when others => invalid_section;
					end case;
					
				when SEC_INIT => null; -- CS: should never happen
			end case;
		end if;

		exception when event: others =>
			log (text => "file " & file_name & space 
				& affected_line (line) & to_string (line), console => true);
			raise;
		
	end process_line;

	procedure read_submodule_files is
	-- Pointer module_cursor points to the last module that has been read.
	-- Take a copy of the submodules stored in module.submods. 
	-- Then iterate in that copy (submods) to read the actual 
	-- module files (like templates/clock_generator.mod).
	-- NOTE: The parent procedure "read_module_file" calls itself here !

		use et_submodules;
		use pac_submodules;

		-- Here the copy of submodules lives:
		submods : et_submodules.pac_submodules.map;
		
		procedure get_submodules (
		-- Copies the submodules in submods.
			module_name	: pac_module_name.bounded_string;
			module		: et_schematic.type_module) is
		begin
			submods := module.submods;
		end;

		procedure query_module (cursor : in pac_submodules.cursor) is begin
			-- Read the template file:
			read_module (to_string (element (cursor).file), log_threshold + 1);
		end;
		
	begin -- read_submodule_files
		-- take a copy of submodules
		query_element (
			position	=> module_cursor,
			process		=> get_submodules'access);

		if length (submods) > 0 then
			log (text => "submodules/templates ...", level => log_threshold);
			log_indentation_up;
		
			-- Query submodules of the parent module (accessed by module_cursor):
			iterate (submods, query_module'access);

			log_indentation_down;
		end if;

	end read_submodule_files;

	procedure test_assembly_variants_of_submodules is
	-- Tests whether the submodules provides the assembly variants as 
	-- specified in module file section ASSEMBLY_VARIANTS.

		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in et_schematic.type_module) is

			use et_assembly_variants;
			use et_assembly_variants.pac_assembly_variants;
			
			variant_cursor : et_assembly_variants.pac_assembly_variants.cursor := module.variants.first;
			variant_name : et_general.pac_assembly_variant_name.bounded_string; -- low_cost

			procedure query_submodules (
				variant_name	: in et_general.pac_assembly_variant_name.bounded_string;
				variant			: in et_assembly_variants.type_assembly_variant) is
				use pac_submodule_variants;
				submod_cursor : pac_submodule_variants.cursor := variant.submodules.first;
				submod_name : pac_module_instance_name.bounded_string; -- CLK_GENERATOR
				submod_variant : et_general.pac_assembly_variant_name.bounded_string; -- fixed_frequency
			begin -- query_submodules
				if submod_cursor = pac_submodule_variants.no_element then
					log (text => "no submodule variants specified", level => log_threshold + 1);
				else
					-- iterate variants of submodules
					while submod_cursor /= pac_submodule_variants.no_element loop
						submod_name := key (submod_cursor); -- CLK_GENERATOR
						submod_variant := element (submod_cursor).variant;
						
						log (text => "submodule instance " & 
								enclose_in_quotes (to_string (submod_name)) &
								" variant " & 
								enclose_in_quotes (to_variant (submod_variant)),
								level => log_threshold + 2);

						if not exists (module_cursor, submod_name, submod_variant) then
							log (ERROR, "submodule " &
								enclose_in_quotes (to_string (submod_name)) &
								" does not provide assembly variant " &
								enclose_in_quotes (to_variant (submod_variant)) & " !",
								console => true);

							log (text => "Look up section " & section_assembly_variants (2..section_assembly_variants'last) &
									" to fix the issue !");
							
							raise constraint_error;
						end if;

						next (submod_cursor);
					end loop;
				end if;
			end query_submodules;
			
		begin -- query_variants
			if variant_cursor = et_assembly_variants.pac_assembly_variants.no_element then
				log (text => "no variants specified", level => log_threshold);
			else
				-- iterate assembly variants of parent module
				while variant_cursor /= et_assembly_variants.pac_assembly_variants.no_element loop
					variant_name := key (variant_cursor);

					-- show assembly variant of parent module
					log (text => "variant " & enclose_in_quotes (to_variant (variant_name)), level => log_threshold + 1);
					log_indentation_up;

					-- look up the submodule variants
					query_element (
						position	=> variant_cursor,
						process		=> query_submodules'access);

					log_indentation_down;
					
					next (variant_cursor);
				end loop;
			end if;
		end;
		
	begin -- test_assembly_variants_of_submodules
		log (text => "verifying assembly variants of submodules ...", level => log_threshold);
		log_indentation_up;

		query_element (
			position	=> module_cursor,
			process		=> query_variants'access);

		log_indentation_down;
	end test_assembly_variants_of_submodules;

	
	use ada.directories;
	
begin -- read_module
	log (text => "opening module file " & enclose_in_quotes (file_name) & " ...", level => log_threshold);
	--log (text => "full name " & enclose_in_quotes (file_name_expanded), level => log_threshold + 1);
	log_indentation_up;
	
	-- Make sure the module file exists.
	-- The file_name may contain environment variables (like $templates). 
	-- In order to test whether the given module file exists, file name_name must be expanded
	-- so that the environment variables are replaced by the real paths like:
	-- templates/clock_generator.mod or
	-- /home/user/et_templates/pwr_supply.mod.
	if exists (file_name_expanded) then

		log (text => "expanded name: " & enclose_in_quotes (full_name (file_name_expanded)),
				level => log_threshold + 1);
		
		-- Create an empty module named after the module file (omitting extension *.mod).
		-- So the module names are things like "motor_driver", "templates/clock_generator" or
		-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
		pac_generic_modules.insert (
			container	=> generic_modules,
			key			=> to_module_name (remove_extension (file_name)),
			position	=> module_cursor,
			inserted	=> module_inserted);

		-- If the module is new to the collection of generic modules,
		-- then open the module file file and read it. 
		-- Otherwise notify operator that module has already been loaded.			 
		if module_inserted then
			
			-- open module file
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name_expanded);
			
			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);
			
			-- read the file line by line
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> ada.text_io.line (current_input),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (text => message_warning & write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Pointer module_cursor points to the last module that has been read.		
			-- The names of submodule/template files are stored in module.submods.file.
			-- But the files itself have not been read. That is what we do next:
			read_submodule_files;

			-- Test existence of assembly variants of submodules.
			test_assembly_variants_of_submodules;
			
		else
			log (text => "module " & enclose_in_quotes (file_name) &
					" already loaded -> no need to load anew.", level => log_threshold + 1);
		end if;
		
	else -- module file not found
		raise semantic_error_1 with
			"ERROR: Module file " & enclose_in_quotes (file_name) & " not found !";
	end if;

	log_indentation_down;
	
	exception when event: others =>
		if is_open (file_handle) then close (file_handle); end if;
		set_input (previous_input);
		raise;
	
end read_module;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
