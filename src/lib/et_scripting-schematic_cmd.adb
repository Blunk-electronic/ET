------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        SCRIPTING IN SCHEMATIC                            --
--                                                                          --
--                               B o d y                                    --
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

with et_modes.schematic;
with et_project.modules;
with et_canvas_schematic_units;
with et_canvas_schematic_nets;
with et_scripting_interactive_schematic;
with et_symbols;
with et_schematic_ops.netlists;
with et_schematic_ops.grid;
with et_schematic_ops.submodules;
with et_board_ops.grid;
with et_pcb;

with et_coordinates_2;
with et_schematic;
with et_sheets;
with et_net_labels;						use et_net_labels;
with et_nets;							use et_nets;
with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;
with et_schematic_ops.nets;
with et_schematic_ops.units;
with et_submodules;
with et_assembly_variants;
-- with et_pick_and_place;
with et_netlists;
with et_devices;
with et_device_placeholders;

with et_canvas.cmd;



separate (et_scripting)
	
procedure schematic_cmd (
	module_cursor	: in pac_generic_modules.cursor;
	cmd_in			: in type_fields_of_line; -- "schematic motor_driver draw net motor_on 1 150 100 150 130"
	log_threshold	: in type_log_level)
is
	use et_project;
	use et_schematic_ops;
	use et_schematic_ops.nets;
	use et_schematic_ops.units;
	use et_schematic_ops.netlists;

	use et_coordinates_2;
	use pac_geometry_2;

	use pac_text_schematic;
	use et_devices;
	use et_canvas_schematic_2;
	use et_canvas_schematic_2.pac_canvas;
	use et_display.schematic;
	use et_modes.schematic;

	package pac_canvas_cmd is new et_canvas_schematic_2.pac_canvas.cmd;
	use pac_canvas_cmd;
	
	
	module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)

	-- In order to tell the command processor that an operation is meant to 
	-- apply to the current sheet, we use the UNIX-bash-like period character:
	here : constant string := ".";

	
	-- This function is a shortcut to get a single field
	-- from the current command:
	function f (place : in type_field_count) return string is begin
		return get_field (single_cmd_status.cmd, place);
	end;


	-- This procedure parses a zoom related command.
	-- If the runmode is non-graphical (like headless) then
	-- nothing will be done here:
	procedure zoom_all is
	begin
		-- log (text => "zoom all ...", level => log_threshold + 1);

		-- Zoom commands can only be executed in a graphical runmode:
		case runmode is
			when MODE_MODULE =>

				case noun is
					when NOUN_ALL => -- zoom all
						case cmd_field_count is
							when 4 => 
								log (text => "zoom all", level => log_threshold + 1);
								zoom_to_fit_all;

							when 5 .. type_field_count'last => too_long;

							when others => command_incomplete;
						end case;

					when others => 
						null;

				end case;

				
			when others =>
					skipped_in_this_runmode (log_threshold + 1);
					
		end case;				
	end zoom_all;

	


	procedure set_grid is 
		use et_schematic_ops.grid;
	begin
		-- Set the grid on the canvas:
		parse_canvas_command (VERB_SET, NOUN_GRID);

		-- The global variable "grid" has now been set
		-- as requested by the operator.
		
		-- Assign the grid in the database:
		set_grid (
			module_name 	=> module,
			grid			=> pac_canvas.grid,
			log_threshold	=> log_threshold + 1);

	end set_grid;
	


	procedure set_scale is begin

		parse_canvas_command (VERB_SET, NOUN_SCALE);
		
		-- The global scale variable "M" has now been set
		-- as requested by the operator.
		
		-- CS: scale_objects (see demo program)

		-- CS: Assign the scale in the database.
	end set_scale;



	procedure create_assembly_variant is
		use et_assembly_variants;
	begin
		case cmd_field_count is
			when 5 =>
				create_assembly_variant
					(
					module_name		=> module,
					variant_name	=> to_variant (f (5)),
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end create_assembly_variant;
	


	procedure delete_assembly_variant is
		use et_assembly_variants;
	begin
		case cmd_field_count is
			when 5 =>
				delete_assembly_variant
					(
					module_name		=> module,
					variant_name	=> to_variant (f (5)),
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;

	end delete_assembly_variant;



	procedure describe_assembly_variant is
		use et_assembly_variants;
	begin
		case cmd_field_count is
			when 6 =>
				describe_assembly_variant
					(
					module_name		=> module,
					variant_name	=> to_variant (f (5)), -- low_cost
					description		=> et_assembly_variants.to_unbounded_string (f (6)), -- "the cheap version"
					log_threshold	=> log_threshold + 1);
				
			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end describe_assembly_variant;


	

	procedure add_device is
		use et_sheets;
	begin
		case cmd_field_count is
			when 9 =>
				-- If a virtual device is added, then no variant is required.
				add_device (
					module_name 	=> module,
					device_model	=> to_file_name (f (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (f (6)),
						point => type_vector_model (set 
									(
									x => to_distance (f (7)),
									y => to_distance (f (8))
									)),
						rotation => to_rotation (f (9))
						),
					variant			=> to_variant_name (""),
					log_threshold	=> log_threshold + 1
					);

			when 10 =>
				-- A real device requires specification of a package variant.
				add_device (
					module_name 	=> module,
					device_model	=> to_file_name (f (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (f (6)),
						point => type_vector_model (set 
									(
									x => to_distance (f (7)),
									y => to_distance (f (8))
									)),
						rotation		=> to_rotation (f (9))
						),
					variant			=> to_variant_name (f (10)),
					log_threshold	=> log_threshold + 1
					);

			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;


	end add_device;


	
	
	procedure add_netchanger is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 8 =>
				add_netchanger (
					module_name 	=> module,
					place			=> to_position 
						(
						sheet => to_sheet (f (5)),
						point => type_vector_model (set 
									(
									x => to_distance (f (6)),
									y => to_distance (f (7))
									)),
						rotation		=> to_rotation (f (8))
						),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end add_netchanger;
	


	procedure move_netchanger is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				move_netchanger
					(
					module_name 	=> module,
					index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3, ...
					coordinates		=> to_coordinates (f (6)),  -- relative/absolute
					sheet			=> to_sheet_relative (f (7)),
					point			=> type_vector_model (set (
										x => to_distance (f (8)),
										y => to_distance (f (9)))),
						
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end move_netchanger;
		
	
	
	procedure delete_netchanger is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 5 =>
				delete_netchanger
					(
					module_name		=> module,
					index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
					log_threshold		=> log_threshold + 1);

			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_netchanger;
	
	

	procedure drag_netchanger is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 8 =>
				drag_netchanger (
					module_name 	=> module,
					index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
					coordinates		=> to_coordinates (f (6)), -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (f (7)),
										y => to_distance (f (8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end drag_netchanger;


	
	procedure rotate_netchanger is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 7 =>
				rotate_netchanger (
					module_name 	=> module,
					index			=> et_submodules.to_netchanger_id (f (5)), -- 1,2,3,...
					coordinates		=> to_coordinates (f (6)), -- relative/absolute
					rotation		=> to_rotation (f (7)), -- 90
					log_threshold	=> log_threshold + 1
					);

			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end rotate_netchanger;
	
	
	
	procedure add_port_to_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				add_port (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					port_name		=> to_net_name (f (6)),
					position		=> type_vector_model (set 
								(
								x => to_distance (f (7)),
								y => to_distance (f (8))
								)),
					direction		=> et_submodules.to_port_name (f (9)),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;

	end add_port_to_submodule;


	
	procedure drag_port_of_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				drag_port (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					port_name		=> to_net_name (f (6)),
					coordinates		=> to_coordinates (f (7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (f (8)),
								y => to_distance (f (9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end drag_port_of_submodule;
	

	
	procedure delete_port_of_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 6 =>
				delete_port
					(
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					port_name		=> to_net_name (f (6)),
					log_threshold	=> log_threshold + 1
					);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_port_of_submodule;
	


	procedure move_port_of_submodule is 
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				move_port (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					port_name		=> to_net_name (f (6)),
					coordinates		=> to_coordinates (f (7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (f (8)),
								y => to_distance (f (9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end move_port_of_submodule;
	
	
	
	procedure add_submodule is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 11 =>
				add_submodule (
					module_name 	=> module, -- parent module (where the submodule is to be inserted)
					file			=> et_submodules.to_submodule_path (f (5)),
					instance		=> et_general.to_instance_name (f (6)), -- submodule instance name
					position		=> to_position 
						(
						sheet => to_sheet (f (7)),
						point => type_vector_model (set 
									(
									x => to_distance (f (8)),
									y => to_distance (f (9))
									))
						),
					size => (
						x => to_distance (f (10)),
						y => to_distance (f (11))
						),
					log_threshold	=> log_threshold + 1
					);

			when 12 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end add_submodule;



	procedure move_submodule is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				move_submodule (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					coordinates		=> to_coordinates (f (6)),  -- relative/absolute
					sheet			=> to_sheet_relative (f (7)),
					point			=> type_vector_model (set (
								x => to_distance (f (8)),
								y => to_distance (f (9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end move_submodule;
		

	
	
	procedure drag_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 8 =>
				drag_submodule (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					coordinates		=> to_coordinates (f (6)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (f (7)),
								y => to_distance (f (8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end drag_submodule;
	

	
	procedure copy_submodule is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				copy_submodule (
					module_name 	=> module, -- parent module (where the submodule is to be copied)
					instance_origin	=> et_general.to_instance_name (f (5)), -- submodule instance name
					instance_new	=> et_general.to_instance_name (f (6)), -- submodule instance name
					destination		=> to_position 
						(
						sheet => to_sheet (f (7)),
						point => type_vector_model (set
									(
									x => to_distance (f (8)),
									y => to_distance (f (9))
									))
						),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end copy_submodule;
	


	procedure delete_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 5 =>
				delete_submodule (
					module_name 	=> module, -- parent module (where the submodule is to be deleted)
					instance		=> et_general.to_instance_name (f (5)), -- submodule instance name
					log_threshold	=> log_threshold + 1
					);

			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_submodule;

	

	procedure rename_submodule is
		use et_sheets;
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 6 =>
				rename_submodule
					(
					module_name		=> module,
					instance_old	=> et_general.to_instance_name (f (5)), -- OSC1
					instance_new	=> et_general.to_instance_name (f (6)), -- OSC2
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end rename_submodule;



	procedure set_submodule_file is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 6 =>
				set_submodule_file (
					module_name 	=> module,
					instance		=> et_general.to_instance_name (f (5)),
					file			=> et_submodules.to_submodule_path (f (6)),
					log_threshold	=> log_threshold + 1
					);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end set_submodule_file;

	
	
	procedure build_submodules_tree is
	begin
		case cmd_field_count is
			when 4 =>
				build_submodules_tree (
					module_name 	=> module,
					log_threshold	=> log_threshold + 1
					);

			when 5 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end build_submodules_tree;
	


	procedure check_integrity is
	begin
		case cmd_field_count is
			when 4 =>
				check_integrity (
					module_name 	=> module,
					log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end check_integrity;



	procedure copy_device is
		use et_sheets;
	begin
		case cmd_field_count is
			when 9 =>
				copy_device (
					module_name 	=> module,
					device_name		=> to_device_name (f (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (f (6)),
						point => type_vector_model (set
									(
									x => to_distance (f (7)),
									y => to_distance (f (8))
									)),
						rotation		=> to_rotation (f (9))
						),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end copy_device;

	
	
	-- For showing and finding devices and units:
	type type_show_device is (
		FIRST_UNIT,
		BY_UNIT_NAME,
		FIRST_UNIT_ON_CURRENT_SHEET);

	
	-- Selects the device so that a certain unit or all its units become
	-- highlighted in the canvas.
	-- Sets the sheet where the unit is.
	-- Pans the canvas so that the unit is in the center of the view.
	-- 1. If mode is FIRST_UNIT then the sheet where the first unit is
	--    will be shown in the center of the canvas. All units of the
	--    device will be selected and highlighted.
	--    The given unit name will be ignored.
	-- 2. If mode is BY_UNIT_NAME then the sheet where the given unit is
	--    will be shown in the center of the canvas. Only that unit
	--    of the device will be selected and highlighted.
	-- 3. If mode is FIRST_UNIT_ON_CURRENT_SHEET then the first unit
	--    on the current sheet will be shown in the center of the canvas.
	--    All units of the device selected and highlighted.
	--    The given unit name will be ignored.
	procedure show_device ( -- GUI related
		device	: in et_devices.type_device_name; -- IC45
		unit	: in et_devices.pac_unit_name.bounded_string := to_unit_name (""); -- A, B, ..
		mode	: in type_show_device := FIRST_UNIT)
	is
		use et_devices;
		use et_canvas_schematic_2;

		function locate (unit : in et_devices.pac_unit_name.bounded_string) 
			return type_unit_query
		is begin
			return unit_position (
				module_cursor	=> current_active_module,
				device_name		=> device,
				unit_name		=> unit);
			
		end locate;

		use et_canvas_schematic_units;
		use pac_proposed_units;

		
		procedure device_not_found is begin
			raise semantic_error_1 with
				"ERROR: Device " & to_string (device) & " does not exist !";
		end device_not_found;

		
		procedure unit_not_found is begin
			raise semantic_error_1 with
				"ERROR: Device " & to_string (device)
				& " unit " & to_string (unit) & " does not exist !";
		end unit_not_found;

		
	begin -- show_device
		case mode is
			when FIRST_UNIT =>
				declare
					-- The unit name is empty because we will center just 
					-- on the first unit:
					location : type_unit_query := locate (to_unit_name (""));
				begin
					if location.exists then
						-- show the sheet where the unit is:
						current_active_sheet := get_sheet (location.position);

						-- center on the first unit
					-- CS center_on (canvas, location.position.place);

						-- Make the whole device (with all its units) selected:
						proposed_units.append (new_item =>
							(
							device	=> locate_device (current_active_module, device),
							unit	=> et_schematic.pac_units.no_element
							));

						selected_unit := proposed_units.first;

						show_properties_of_selected_device;
					else
						device_not_found;
					end if;
				end;

			when BY_UNIT_NAME =>
				declare
					-- The unit name is explicitely given:
					location : type_unit_query := locate (unit);
				begin
					if location.exists then
						-- show the sheet where the unit is:
						current_active_sheet := get_sheet (location.position);

						-- center on the unit
					-- CS center_on (canvas, location.position.place);

						-- Make the whole device (with all its units) selected:
						proposed_units.append (new_item =>
							(
							device	=> locate_device (current_active_module, device),
							unit	=> locate_unit (current_active_module, device, unit)
							));

						selected_unit := proposed_units.first;

						show_properties_of_selected_device;
					else
						unit_not_found;
					end if;
				end;

			when FIRST_UNIT_ON_CURRENT_SHEET =>
				declare
					-- The unit name is empty because we will center just 
					-- on the first unit on the current sheet:
					location : type_unit_query := locate (to_unit_name (""));
					use et_sheets;
				begin
					if location.exists then
						if get_sheet (location.position) = current_active_sheet then

							-- center on the unit
						-- CS	center_on (canvas, location.position.place);

							-- Make the whole device (with all its units) selected:
							proposed_units.append (new_item =>
								(
								device	=> locate_device (current_active_module, device),
								unit	=> et_schematic.pac_units.no_element
								));

							selected_unit := proposed_units.first;
							
							show_properties_of_selected_device;
						else
							raise semantic_error_1 with
								"Device " & to_string (device) & " is not on this sheet !";
						end if;

					else
						device_not_found;
					end if;
				end;
				
		end case;
	end show_device;



	procedure delete_device is
	begin
		case cmd_field_count is
			when 5 =>
				delete_device (
					module_name 	=> module,
					device_name		=> to_device_name (f (5)),
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_device;

	

	procedure delete_unit is
	begin
		case cmd_field_count is
			when 6 =>
				delete_unit (
					module_name 	=> module,
					device_name		=> to_device_name (f (5)),
					unit_name		=> to_unit_name (f (6)),
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_unit;
	


	procedure drag_unit is
	begin
		case cmd_field_count is
			when 9 =>
				drag_unit
					(
					module_name 	=> module,
					device_name		=> to_device_name (f (5)),
					unit_name		=> to_unit_name (f (6)),
					coordinates		=> to_coordinates (f (7)), -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (f (8)),
										y => to_distance (f (9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end drag_unit;
	


	procedure fetch_unit is
		use et_sheets;
	begin
		case cmd_field_count is
			when 10 =>
				fetch_unit (
					module_name		=> module,
					device_name		=> to_device_name (f (5)),
					unit_name		=> to_unit_name (f (6)),
					destination		=> to_position 
						(
						sheet => to_sheet (f (7)),
						point => type_vector_model (set
									(
									x => to_distance (f (8)),
									y => to_distance (f (9))
									)),
						rotation		=> to_rotation (f (10))
						),
					log_threshold	=> log_threshold + 1
					);

			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end fetch_unit;
	
	
	
	-- For showing and finding nets:
	type type_show_net is (
		FIRST_NET,
		NET_ON_CURRENT_SHEET);


	
	procedure show_net (
		net		: in pac_net_name.bounded_string; -- RESET_N
		mode	: in type_show_net)
	is
		use et_schematic;
		use pac_nets;
		
		use pac_strands;
		
		use et_canvas_schematic_nets;
		use pac_proposed_segments;
		
		net_cursor : pac_nets.cursor := locate_net (current_active_module, net);

		strand_cursor : pac_strands.cursor;
		
		pos : et_coordinates_2.type_position;
	begin
		if net_cursor /= pac_nets.no_element then
			
			case mode is
				when FIRST_NET =>
					-- Get the cursor of the first strand:
					strand_cursor := get_first_strand (net_cursor);

					-- show the sheet where the first net is:
					current_active_sheet := get_sheet (element (strand_cursor).position);

					-- center drawing where the strand starts:
				-- CS center_on (canvas, element (strand_cursor).position.place);
					
					proposed_segments.append (new_item => (
						net		=> net_cursor,
						strand	=> strand_cursor,
						segment	=> get_first_segment (strand_cursor)));

					selected_segment := proposed_segments.first;
					
					show_properties_of_selected_net;
					
				when NET_ON_CURRENT_SHEET =>
					-- Get the cursor of the first strand on the current
					-- active sheet:
					strand_cursor := get_first_strand_on_sheet (
										current_active_sheet, net_cursor);

					-- If the net does have a strand on the given sheet,
					-- select it and show properties in status bar.
					if strand_cursor /= pac_strands.no_element then

						-- center drawing where the strand starts:
					-- CS center_on (canvas, element (strand_cursor).position.place);

						proposed_segments.append (new_item => (
							net		=> net_cursor,
							strand	=> strand_cursor,
							segment	=> get_first_segment (strand_cursor)));

						selected_segment := proposed_segments.first;
						
						show_properties_of_selected_net;
					else
						raise semantic_error_1 with
							"ERROR: Net " & enclose_in_quotes (to_string (net))
							& " is not on this sheet !";
					end if;
			end case;
		else
			raise semantic_error_1 with
				"ERROR: Net " & enclose_in_quotes (to_string (net)) & " does not exist !";
		end if;
	end show_net;



	procedure delete_net_label is
		use et_sheets;
	begin
		case cmd_field_count is
			when 7 =>
				delete_net_label
					(
					module_cursor	=> current_active_module,

					position		=> to_position (
										point => type_vector_model (set (
											x => to_distance (f (6)),
											y => to_distance (f (7)))),
										sheet => to_sheet (f (5))), -- sheet number
					
					log_threshold	=> log_threshold + 1);
				
			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_net_label;



	procedure delete_net is
		use et_sheets;
	begin
		case cmd_field_count is

			-- If the statement has only 6 fields, the net scope is EVERYWHERE.
			-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
			-- by the called procedure:
			when 5 =>
				delete_net
					(
					module_cursor		=> current_active_module,
					net_name			=> to_net_name (f (5)), -- RESET
					scope				=> EVERYWHERE,
					place				=> to_position (
											point => origin,
											sheet => 1),
					log_threshold		=> log_threshold + 1);

			-- If the statement has 7 fields, the net scope is SHEET.
			-- Sheet is set by the 7th argument. x and y assume default (0/0)
			-- and are further-on ignored by the called procedure:
			when 6 =>
				delete_net
					(
					module_cursor		=> current_active_module,
					net_name			=> to_net_name (f (5)), -- RESET
					scope				=> SHEET,
					place				=> to_position (
											point => origin,
											sheet => to_sheet (f (6))), -- sheet number
					log_threshold		=> log_threshold + 1);

			-- If the statement has 9 fields, the net scope is STRAND.
			-- Place is set according to arguments 7..9.
			when 8 =>
				delete_net
					(
					module_cursor		=> current_active_module,
					net_name			=> to_net_name (f (5)), -- RESET
					scope				=> STRAND,
					place				=> to_position (
											point => type_vector_model (set (
												x => to_distance (f (7)),
												y => to_distance (f (8)))),
											sheet => to_sheet (f (6))), -- sheet number
					log_threshold		=> log_threshold + 1);

				
			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;

	end delete_net;



	procedure delete_net_segment is
		use et_sheets;
	begin
		case cmd_field_count is
			when 8 =>
				delete_segment
					(
					module_cursor	=> current_active_module,
					net_name		=> to_net_name (f (5)), -- RESET
					place			=> to_position (
										point => type_vector_model (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										sheet => to_sheet (f (6))), -- sheet number
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_net_segment;

	

	procedure drag_net_segment is
		use et_sheets;
	begin
		case cmd_field_count is
			when 11 =>
				drag_segment
					(
					module_cursor	=> current_active_module,
					net_name		=> to_net_name (f (5)), -- RESET
					point_of_attack	=> to_position (
										point => type_vector_model (set (
											x => to_distance (f (7)),
											y => to_distance (f (8)))),
										sheet => to_sheet (f (6))), -- sheet number
					
					coordinates		=> to_coordinates (f (9)), -- relative/absolute
					
					destination		=> type_vector_model (set (
										x => to_distance (f (10)),
										y => to_distance (f (11)))),
					
					log_threshold	=> log_threshold + 1);

			when 12 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end drag_net_segment;
	
	
	
	-- This procedure extracts from the command the
	-- sheet number and sets it active.
	-- It updates the sheet number display accordingly:
	procedure show_sheet is -- GUI related
		use et_sheets;
		sheet : type_sheet;

		procedure show is
		begin
			sheet := to_sheet (f (5));

			-- CS test whether sheet exists
			
			current_active_sheet := sheet;
			update_sheet_number_display;
		end show;
			
	begin
		log (text => "set sheet" & to_sheet (sheet), level => log_threshold + 1); 

		update_mode_display;
		
		case cmd_field_count is
			when 5 => show;
			when 6 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
		
	end show_sheet;



	procedure create_module is
		use et_project.modules;

		
		procedure do_it (
			module_name : in pac_module_name.bounded_string) 
		is
			use pac_generic_modules;
		begin
			create_module (
				module_name		=> module_name, -- led_driver_test
				log_threshold	=> log_threshold + 1);

			-- Show the module in schematic and board editor:
			
			current_active_module := locate_module (module_name);
			current_active_sheet := 1;

			-- Update module name in the schematic window title bar:
			-- CS set_title_bar (active_module);
			
			-- CS update_sheet_number_display;
			
			-- Update the board window title bar:
			-- CS et_canvas_board_2.set_title_bar (active_module);
		end do_it;

		
	begin
		case cmd_field_count is
			when 5 => do_it (to_module_name (f (5)));
			when 6 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
	end create_module;


	
	-- This procedure extracts from the command the
	-- name of the generic module and optionally the
	-- sheet number.
	-- It sets the given module and sheet as active
	-- and updates the editor window according
	-- to the activated module:
	procedure show_module is  -- GUI related

		module : pac_module_name.bounded_string;

		use et_sheets;
		sheet : type_sheet := 1;

		
		-- Sets the active module and first sheet.
		procedure module_and_first_sheet is begin
			module := to_module_name (f (5));
			set_module (module);
			current_active_sheet := sheet;
			
			update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_first_sheet;



		-- Sets the active module and sheet.
		procedure module_and_random_sheet is begin
			module := to_module_name (f (5));
			set_module (module);

			log (text => "sheet " & to_sheet (sheet), 
				level => log_threshold + 1);
			
			sheet := to_sheet (f (6));
			current_active_sheet := sheet;

			update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_random_sheet;
		
		
	begin
		log (text => "show module (via schematic editor) " 
			 & enclose_in_quotes (to_string (module)),
			 level => log_threshold + 1);

		update_mode_display;
		
		case cmd_field_count is
			when 5 => module_and_first_sheet; -- show module LED-driver
			when 6 => module_and_random_sheet; -- show module LED-driver 2
			when 7 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
		
	end show_module;

	
	



	-- Enables a certain layer. If status is empty, the layer will be enabled.
	procedure display is -- GUI related

		
		procedure do_it ( 
			layer	: in type_noun;
			status	: in string := "") is

			ls : type_layer_status;
		begin
			-- Convert the given status to type_layer_status.
			-- If no status given, assume status ON:
			if status = "" then
				ls := ON;
			else
				ls := to_layer_status (status);
			end if;
			
			log (text => "display " & to_lower (to_string (layer)) 
					& space & to_string (ls),
					level => log_threshold + 1);
			
			case layer is
				when NOUN_NAMES		=> layers.device_names := ls;
				when NOUN_NETS		=> layers.nets := ls;
				when NOUN_PORTS		=> layers.ports := ls;
				when NOUN_PURPOSES	=> layers.device_purposes := ls;
				when NOUN_TEXTS		=> layers.texts := ls;
				when NOUN_VALUES	=> layers.device_values := ls;
				
				when others => 
					log (importance => ERROR, text => "invalid layer !", console => true);
			end case;

			-- CS exception handler if status is invalid
		end do_it;

		
	begin
		case cmd_field_count is
			when 4 => do_it (noun); -- if status is omitted
			when 5 => do_it (noun, f (5));
			when 6 .. type_field_count'last => too_long; 
			when others => command_incomplete;
		end case;
	end display;
	

	

	procedure delete_module is

		use et_project.modules;
		use pac_generic_modules;

		
		-- Delete the current active module:
		procedure delete_active is begin
			delete_module (
				module_name		=> key (module_cursor),
				log_threshold	=> log_threshold + 1);

			-- As long as there are other modules, open the 
			-- first of the generic modules.
			-- If no modules available any more, close the schematic
			-- and board editor:

			-- CS Set the previous module active instead ?
			if length (generic_modules) > 0 then
				
				current_active_module := generic_modules.first;
				current_active_sheet := 1;

				log (text => "set module " 
					 & enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (current_active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board_2.set_title_bar (current_active_module);
			else
				-- CS
				null;
				-- terminate_main;
			end if;
		end delete_active;


		
		procedure delete_explicit (
			module_name : in pac_module_name.bounded_string) 
		is begin
			delete_module (
				module_name		=> module_name, -- led_driver_test
				log_threshold	=> log_threshold + 1);

			-- As long as there are other modules, open the 
			-- first of the generic modules.
			-- If no modules available any more, close the schematic
			-- and board editor:
			
			-- CS Set the previous module active instead ?
			if length (generic_modules) > 0 then
			
				current_active_module := generic_modules.first;
				current_active_sheet := 1;

				log (text => "set module " 
					 & enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (current_active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board_2.set_title_bar (current_active_module);
			else
				-- CS
				null;
				-- terminate_main;
			end if;
		end delete_explicit;

		
	begin
		case cmd_field_count is
			when 4 => delete_active;								
			when 5 => delete_explicit (to_module_name (f (5)));
			when 6 .. type_field_count'last => too_long;								
			when others => command_incomplete;
		end case;
	end delete_module;


	
	-- Actions to save a module:
	procedure save_module is 
	begin
		-- Since we are already in the project directory,
		-- we can call the save_module procedures right away.
		
		case cmd_field_count is
			when 4 =>
				-- Save the module with its own name:
				save_module (
					module_cursor	=> current_active_module,
					log_threshold	=> log_threshold + 1);

			when 5 =>
				-- Save the module with a different name:
				save_module (
					module_cursor	=> current_active_module,
					save_as_name	=> to_module_name (f (5)), -- led_driver_test
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;			

	end save_module;

	
	


	
	-- Parses the single_cmd_status.cmd:
	procedure parse is 
		use et_project.modules;
		use et_sheets;
		use et_device_placeholders;
		use et_assembly_variants;
	begin
		log (text => "parsing command: " 
			& enclose_in_quotes (to_string (single_cmd_status.cmd)),
			level => log_threshold);

		-- Clear the status bar if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			status_clear;
		end if;

		
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						add_device;
						
					when NOUN_NETCHANGER =>
						add_netchanger;
						
					when NOUN_PORT =>
						add_port_to_submodule;
						
					when NOUN_SUBMODULE =>
						add_submodule;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_BUILD =>
				case noun is
					when NOUN_SUBMODULES_TREE =>
						build_submodules_tree;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_CHECK =>
				case noun is
					when NOUN_INTEGRITY =>
						check_integrity;
							
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_COPY =>
				case noun is
					when NOUN_DEVICE =>
						copy_device;
						
					when NOUN_SUBMODULE =>
						copy_submodule;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_CREATE =>
				case noun is
					when NOUN_VARIANT => 
						create_assembly_variant;

					when NOUN_MODULE =>
						create_module;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_DELETE =>
				case noun is
					when NOUN_DEVICE =>
						delete_device;
						
					when NOUN_LABEL =>
						delete_net_label;
						
					when NOUN_MODULE =>
						delete_module;
						
					when NOUN_NET =>
						delete_net;
						
					when NOUN_NETCHANGER =>
						delete_netchanger;
						
					when NOUN_PORT =>
						delete_port_of_submodule;
						
					when NOUN_SEGMENT =>
						delete_net_segment;
						
					when NOUN_SUBMODULE =>
						delete_submodule;
						
					when NOUN_TEXT =>
						NULL; -- CS
						
					when NOUN_UNIT =>
						delete_unit;
						
					when NOUN_VARIANT => 
						delete_assembly_variant;
						
					when others => invalid_noun (to_string (noun));
				end case;
				
				
			when VERB_DESCRIBE =>
				case noun is
					when NOUN_VARIANT => 
						describe_assembly_variant;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_DISPLAY => -- GUI related
				case noun is
					when NOUN_PORTS		-- like "schematic led_driver display ports [on/off]"
						| NOUN_NETS		-- like "schematic led_driver display nets [on/off]"
						| NOUN_NAMES | NOUN_VALUES | NOUN_PURPOSES
						| NOUN_TEXTS
						=> display;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_DRAG =>
				case noun is
					when NOUN_UNIT =>
						drag_unit;
								
					when NOUN_NETCHANGER =>
						drag_netchanger;

					when NOUN_PORT =>
						drag_port_of_submodule;
						
					when NOUN_SEGMENT =>
						drag_net_segment;
						
					when NOUN_SUBMODULE =>
						drag_submodule;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						case cmd_field_count is
							when 10 =>
								insert_net
									(
									module_cursor	=> current_active_module,
									net_name		=> to_net_name (f (5)), -- RESET
									start_point		=> to_position (
															point => type_vector_model (set (
																x => to_distance (f (7)),
																y => to_distance (f (8)))),
															sheet => to_sheet (f (6))), -- sheet number
									
									end_point		=> type_vector_model (set (
														x => to_distance (f (9)),
														y => to_distance (f (10)))),
									
									log_threshold	=> log_threshold + 1);

							when 11 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_EXECUTE =>
				case noun is
					when NOUN_SCRIPT =>
						case cmd_field_count is
							when 5 => 
								execute_nested_script (
									file			=> f (5),
									log_threshold	=> log_threshold + 1);

							when 6 .. type_field_count'last => too_long;								
							when others => command_incomplete;
						end case;
							
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_EXIT | VERB_QUIT => 
				null;
				-- CS terminate_main;
				-- CS does not work via script (gtk error ...)

				
			when VERB_FETCH =>
				case noun is
					when NOUN_UNIT =>
						fetch_unit;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MOVE =>
				case noun is
					when NOUN_CURSOR =>
						parse_canvas_command (VERB_MOVE, NOUN_CURSOR);

						
					when NOUN_NAME =>
						-- schematic led_driver move name R1 1 absolute 10 15
						case cmd_field_count is
							when 9 =>
								move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_vector_model (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> NAME,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_VALUE =>
						case cmd_field_count is
							when 9 =>
								move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_vector_model (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> VALUE,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
						
					when NOUN_PORT =>
						move_port_of_submodule;

								
					when NOUN_PURPOSE =>
						case cmd_field_count is
							when 9 =>
								move_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									point			=> type_vector_model (set (
														x => to_distance (f (8)),
														y => to_distance (f (9)))),
									meaning			=> PURPOSE,
									log_threshold	=> log_threshold + 1
									);

							when 10 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_NETCHANGER =>
						move_netchanger;

								
					when NOUN_TEXT =>
						NULL; -- CS

						
					when NOUN_SUBMODULE =>
						move_submodule;

						
					when NOUN_UNIT =>
						case cmd_field_count is
							when 10 =>
								move_unit
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									sheet			=> to_sheet_relative (f (8)),
									point			=> type_vector_model (set (
														x => to_distance (f (9)),
														y => to_distance (f (10)))),
										
									log_threshold	=> log_threshold + 1
									);

							when 11 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
								
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MAKE =>
				case noun is
					when NOUN_BOM => 
						case cmd_field_count is
							when 4 =>
								make_boms -- a BOM for each variant
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;

					when NOUN_NETLISTS => 
						case cmd_field_count is
							when 4 =>
								make_netlists 
									(
									module_name 	=> module,
									log_threshold	=> log_threshold + 1);

							when 5 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MOUNT =>
				case noun is
					when NOUN_DEVICE => 
						declare
							value : pac_device_value.bounded_string; -- 470R
							partcode : pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
							purpose : pac_device_purpose.bounded_string; -- brightness_control
						begin
							-- validate value
							value := to_value_with_check (f (7));

							-- validate partcode
							partcode := to_partcode (f (8));
							
							case cmd_field_count is
								when 8 =>
									-- set value and partcode
									mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (f (5)), -- low_cost
										device			=> to_device_name (f (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										log_threshold	=> log_threshold + 1);

								when 9 =>
									-- optionally the purpose can be set also
									purpose := to_purpose (f (9)); -- brightness_control
												
									mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (f (5)), -- low_cost
										device			=> to_device_name (f (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										purpose			=> purpose, -- brightness_control
										log_threshold	=> log_threshold + 1);
									
								when 10 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;

						end; -- declare

					when NOUN_SUBMODULE =>
						case cmd_field_count is
							when 7 =>
								mount_submodule
									(
									module_name		=> module,
									variant_parent	=> to_variant (f (5)), -- low_cost
									instance		=> et_general.to_instance_name (f (6)), -- OSC1
									variant_submod	=> to_variant (f (7)), -- fixed_frequency
									log_threshold	=> log_threshold + 1);

							when 8 .. type_field_count'last => too_long;
								
							when others => command_incomplete;

						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_JUNCTION =>
						case cmd_field_count is
							when 7 =>
								place_junction 
									(
									module_cursor 	=> current_active_module,
									place			=> to_position 
														(
														sheet => to_sheet (f (5)),
														point => type_vector_model (set (
																	x => to_distance (f (6)),
																	y => to_distance (f (7))
																	))
														),
										
									log_threshold	=> log_threshold + 1
									);

							when 8 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_LABEL =>
						case cmd_field_count is
							when 10 =>
								-- SIMPLE LABEL
								place_net_label
									(
									module_cursor		=> current_active_module,

									segment_position	=> to_position (
															point => type_vector_model (set (
																x => to_distance (f (6)),
																y => to_distance (f (7)))),
															sheet => to_sheet (f (5))), -- sheet number

									label_position		=> type_vector_model (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),

									rotation			=> to_rotation (f (10)), -- 0 / 90
									appearance 			=> et_net_labels.SIMPLE,

									-- A simple label does not indicate the direction
									-- of information flow. But this procedure call requires a
									-- direction. So we just pass direction PASSIVE. It has no 
									-- further meaning.
									direction			=> PASSIVE,

									log_threshold		=> log_threshold + 1);

							when 8 =>
								-- TAG LABEL
								place_net_label
									(
									module_cursor		=> current_active_module,

									segment_position	=> to_position (
															point => type_vector_model (set (
																x => to_distance (f (6)),
																y => to_distance (f (7)))),
															sheet => to_sheet (f (5))), -- sheet number

									appearance 			=> TAG,

									-- A tag label requires specification of signal direction:
									direction			=> to_direction (f (8)), -- INPUT, OUTPUT, PASSIVE, ...

									log_threshold		=> log_threshold + 1);
								
							when 11 .. type_field_count'last => too_long;
								
							when others => command_incomplete; -- incl. field count of 9
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_REMOVE =>
				case noun is
					when NOUN_DEVICE => 
						case cmd_field_count is
							when 6 =>
								remove_device -- from assembly variant
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_device_name (f (6)), -- R1
									log_threshold	=> log_threshold + 1);

							when 7 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;

					when NOUN_SUBMODULE =>
						case cmd_field_count is
							when 6 =>
								remove_submodule
									(
									module_name		=> module,
									variant_parent	=> to_variant (f (5)),
									instance		=> et_general.to_instance_name (f (6)), -- OSC1
									log_threshold	=> log_threshold + 1);

							when 7 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						case cmd_field_count is
							when 6 =>
								rename_device
									(
									module_name 		=> module,
									device_name_before	=> to_device_name (f (5)), -- IC1
									device_name_after	=> to_device_name (f (6)), -- IC23
									log_threshold		=> log_threshold + 1
									);

							when 7 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case; 

						
					when NOUN_SUBMODULE =>
						rename_submodule;

						
					when NOUN_NET =>
						case cmd_field_count is

							-- If the statement has only 6 fields, the net scope is EVERYWHERE.
							-- Place assumes default (sheet 1, x/y 0/0) and is further-on ignored 
							-- by the called procedure:
							when 6 =>
								rename_net
									(
									module_cursor		=> current_active_module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> EVERYWHERE,
									place				=> to_position (
															point => origin,
															sheet => 1),
									log_threshold		=> log_threshold + 1);

							-- If the statement has 7 fields, the net scope is SHEET.
							-- Sheet is set by the 7th argument. x and y assume default (0/0)
							-- and are further-on ignored by the called procedure:
							when 7 =>
								rename_net
									(
									module_cursor		=> current_active_module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> SHEET,
									place				=> to_position (
															point => origin,
															sheet => to_sheet (f (7))), -- sheet number
									log_threshold		=> log_threshold + 1);

							-- If the statement has 9 fields, the net scope is STRAND.
							-- Place is set according to arguments 7..9.
							when 9 =>
								rename_net
									(
									module_cursor		=> current_active_module,
									net_name_before		=> to_net_name (f (5)), -- RESET
									net_name_after		=> to_net_name (f (6)), -- RESET_N
									scope				=> STRAND,
									place				=> to_position (
															point => type_vector_model (set (
																x => to_distance (f (8)),
																y => to_distance (f (9)))),
															sheet => to_sheet (f (7))), -- sheet number
									log_threshold		=> log_threshold + 1);

								
							when 10 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_RENUMBER =>
				case noun is
					when NOUN_DEVICES =>
						case cmd_field_count is
							when 5 =>
								renumber_devices
									(
									module_name 	=> module,
									step_width		=> to_index (f (5)), -- 100
									log_threshold	=> log_threshold + 1
									);

							when 6 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_ROTATE =>
				case noun is
					when NOUN_TEXT =>
						NULL; -- CS

					when NOUN_UNIT =>
						case cmd_field_count is
							when 8 =>
								rotate_unit
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									coordinates		=> to_coordinates (f (7)),  -- relative/absolute
									rotation		=> to_rotation (f (8)), -- 90
									log_threshold	=> log_threshold + 1
									);

							when 9 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
								
					when NOUN_NAME =>
						case cmd_field_count is 
							when 7 =>
								rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									rotation		=> to_rotation_doc (f (7)), -- 90
									meaning			=> NAME,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
								
					when NOUN_VALUE =>
						case cmd_field_count is
							when 7 =>
								rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									rotation		=> to_rotation_doc (f (7)), -- 90
									meaning			=> VALUE,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;
								
					when NOUN_PURPOSE =>
						case cmd_field_count is
							when 7 =>
								rotate_unit_placeholder
									(
									module_name 	=> module,
									device_name		=> to_device_name (f (5)), -- IC1
									unit_name		=> to_unit_name (f (6)), -- A
									rotation		=> to_rotation_doc (f (7)), -- 90
									meaning			=> PURPOSE,
									log_threshold	=> log_threshold + 1
									);

							when 8 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_NETCHANGER =>
						rotate_netchanger;

						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_SAVE =>
				case noun is
					when NOUN_MODULE =>
						save_module;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_SET =>
				case noun is
					when NOUN_CLASS =>
						case cmd_field_count is
							when 6 =>
								-- schematic led_driver set class GND pwr
								set_net_class (
									module_cursor	=> current_active_module,
									net_name		=> to_net_name (f (5)),
									net_class		=> et_pcb.to_net_class_name (f (6)),
									log_threshold	=> log_threshold + 1);
								
							when 7 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;
						
						
					when NOUN_GRID =>
						set_grid;

					when NOUN_CURSOR =>
						parse_canvas_command (VERB_SET, NOUN_CURSOR);

					when NOUN_ZOOM =>
						parse_canvas_command (VERB_SET, NOUN_ZOOM);
						
					when NOUN_SCALE =>
						set_scale;


						
					when NOUN_PARTCODE =>
						case cmd_field_count is
							when 6 =>
								declare
									partcode : pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
								begin
									partcode := to_partcode (f (6));

									-- set the purpose
									set_partcode
										(
										module_name 	=> module,
										device_name		=> to_device_name (f (5)), -- R1
										partcode		=> partcode, -- R_PAC_S_0805_VAL_100R
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_PURPOSE =>
						case cmd_field_count is
							when 6 =>
								declare
									use et_schematic;
									purpose : pac_device_purpose.bounded_string; -- brightness_control
								begin
									purpose := to_purpose (f (6));
									
									-- set the purpose
									set_purpose
										(
										module_name 	=> module,
										device_name		=> to_device_name (f (5)), -- R1
										purpose			=> purpose, -- brightness_control
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_SCOPE =>
						case cmd_field_count is
							when 6 =>
								set_scope (
									module_cursor 	=> current_active_module,
									net_name		=> to_net_name (f (5)),
									scope			=> et_netlists.to_net_scope (f (6)),
									log_threshold	=> log_threshold + 1
									);

							when 7 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;

						
					when NOUN_SUBMODULE_FILE =>
						set_submodule_file;
						

					when NOUN_VALUE =>
						case cmd_field_count is
							when 6 =>
								declare
									value : pac_device_value.bounded_string; -- 470R
								begin
									-- validate value
									value := to_value_with_check (f (6));

									-- set the value
									set_value
										(
										module_name 	=> module,
										device_name		=> to_device_name (f (5)), -- R1
										value			=> value, -- 470R
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_VARIANT =>
						case cmd_field_count is
							when 6 =>
								declare
									variant : pac_package_variant_name.bounded_string; -- N, D
								begin
									-- validate variant
									check_variant_name_length (f (6));
									variant := to_variant_name (f (6));
									check_variant_name_characters (variant);
									
									-- set the variant
									set_variant
										(
										module			=> module,
										device			=> to_device_name (f (5)), -- IC1
										variant			=> variant, -- N, D
										log_threshold	=> log_threshold + 1
										);
								end;

							when 7 .. type_field_count'last => too_long; 
								
							when others => command_incomplete;
						end case;

						
					when NOUN_TEXT_SIZE =>
						NULL; -- CS
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_SHOW => -- GUI related
				-- There might be objects such as net segments or units selected.
				-- They must be de-selected first:
				clear_proposed_objects;
				
				case noun is
					when NOUN_DEVICE =>
						case cmd_field_count is
							when 5 => show_device ( -- show device R1
									device	=> to_device_name (f (5)), -- R1, IC1
									mode	=> FIRST_UNIT);
							
							when 6 =>
								-- The 6th field may be a period, which means
								-- the unit is to be shown on the current active sheet.
								-- Otherwise the field provides an explicit
								-- unit name:
								if f (6) = here then
									show_device ( -- show device IC1 .
										device	=> to_device_name (f (5)), -- IC1
										mode	=> FIRST_UNIT_ON_CURRENT_SHEET);
								else
									show_device ( -- show device IC1 A
										device	=> to_device_name (f (5)), -- IC1
										unit	=> to_unit_name (f (6)), -- A
										mode	=> BY_UNIT_NAME);
								end if;
								
							when 7 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_MODULE =>
						show_module;

						
					when NOUN_NET =>
						case cmd_field_count is
							when 5 => show_net ( -- show net RESET_N
									net		=> to_net_name (f (5)), -- RESET_N
									mode	=> FIRST_NET);
							
							when 6 =>
								-- The 6th field may be a period, which means
								-- the net is to be shown on the current active sheet.
								-- If the 6th field is not a period, then we
								-- threat this field as excessive argument.
								if f (6) = here then
									show_net ( -- show net RESET_N
										net		=> to_net_name (f (5)), -- RESET_N
										mode	=> NET_ON_CURRENT_SHEET);
								else
									too_long;
								end if;
								
							when 7 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;
						
					when NOUN_SHEET =>
						show_sheet;
					
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_UNMOUNT =>
				case noun is
					when NOUN_DEVICE => 
						case cmd_field_count is
							when 6 =>
								unmount_device
									(
									module_name		=> module,
									variant_name	=> to_variant (f (5)), -- low_cost
									device			=> to_device_name (f (6)), -- R1
									log_threshold	=> log_threshold + 1);

							when 7 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_WRITE =>
				case noun is
					when NOUN_TEXT =>
						NULL; -- CS

					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_ZOOM => -- GUI related
				zoom_all;


			when others =>
				null;
				
		end case;

		
		-- Update GUI if we are in graphical mode:
		if runmode /= MODE_HEADLESS then
			null;
			
		end if;
		
	end parse;		



	
	procedure propose_arguments is
		use et_sheets;
		use et_scripting_interactive_schematic;
		use et_canvas_schematic_units;
		use et_canvas_schematic_nets;
		use et_project.modules;

		device_name		: et_devices.type_device_name;
		unit_name		: pac_unit_name.bounded_string;
		--net_name		: pac_net_name.bounded_string;

		
		procedure module_name_missing is begin
			set_status (incomplete & module_missing);
		end module_name_missing;

		
		procedure device_name_missing is begin
			set_status (incomplete & device_missing);
			-- No menu required and not reasonable.
			-- It might become very long if there were hundreds of devices.
		end device_name_missing;

		
		procedure unit_name_missing is begin
			set_status (incomplete & "Unit name missing !");
		end unit_name_missing;

		
		procedure sheet_number_missing is begin
			set_status (incomplete & "Sheet number missing !");
		end sheet_number_missing;

		
		procedure device_not_found is begin
			set_status ("ERROR: Device " & to_string (device_name) & " not found !");
		end device_not_found;

		
		procedure unit_not_found is begin
			set_status ("ERROR: Device " & to_string (device_name) 
				& " does not provide unit " & to_string (unit_name) & " !");
		end unit_not_found;

		
		procedure unit_not_deployed is begin
			set_status ("ERROR: Unit " & to_string (unit_name) 
				& " of device " & to_string (device_name) 
				& " not deployed !");
		end unit_not_deployed;

		
		procedure unit_in_use is begin
			set_status ("ERROR: Unit " & to_string (unit_name) 
				& " of device " & to_string (device_name) 
				& " already in use !");
			-- CS output coordinates of used unit
		end unit_in_use;

		
		procedure unit_not_on_this_sheet is begin
			set_status ("ERROR: Unit " & to_string (unit_name) & " is not on this sheet !");
		end unit_not_on_this_sheet;

		
		procedure net_name_missing is begin
			set_status (incomplete & net_missing);
		end net_name_missing;



		procedure fetch_unit is
		begin
			case cmd_field_count is
				when 4 =>
					device_name_missing;
					
				when 5 => -- like "fetch unit IC1"
					unit_name_missing;

					device_name := to_device_name (f (5));

					if exists (current_active_module, device_name) then

						unit_add.device		:= device_model_cursor (current_active_module, device_name);
						
						--unit_add.variant	:= device_variant_name (current_active_module, device_name);
						-- CS: really required ? requires test whether the device is real
						
						unit_add.total		:= units_total (unit_add.device);
						unit_add.device_pre	:= device_name;
					
						menu_propose_units_on_fetch (
							device			=> device_name,
							units			=> available_units (
												current_active_module,
												device_name,
												log_threshold + 1),
							log_threshold	=> log_threshold + 1);

					else
						device_not_found;
					end if;

					
				when 6 => -- like "fetch unit IC1 B"
					device_name := to_device_name (f (5));

					if exists (current_active_module, device_name) then

						unit_add.device		:= device_model_cursor (current_active_module, device_name);

						--unit_add.variant	:= device_variant_name (current_active_module, device_name);
						-- CS: really required ? requires test whether the device is real

						unit_add.total		:= units_total (unit_add.device);
						unit_add.device_pre	:= device_name;
						
						unit_name := to_unit_name (f (6));

						-- test existence AND availability of unit:
						if provides_unit (unit_add.device, unit_name) then

							if unit_available (current_active_module, device_name, unit_name) then

								unit_add.name := unit_name;
								
								-- Allow drawing the unit:
								unit_add.via_fetch := true;
							
								-- CS redraw;
							else
								unit_in_use;
							end if;
						else
							unit_not_found;
						end if; 
					else
						device_not_found;
					end if;
										
				when others => null;								
			end case;
		end fetch_unit;
		
		
		
	begin -- propose_arguments
		log_command_incomplete (cmd_field_count, log_threshold);

		-- There might be objects such as net segments or units selected.
		-- They must be de-selected:
		clear_proposed_objects;
		
		case verb is
			when VERB_ADD =>
				case noun is
					when NOUN_DEVICE =>
						set_status (et_canvas_schematic_units.status_add);

						-- open device model selection
						add_device; 

					when others => null; -- CS
				end case;
				

			when VERB_CREATE =>
				case noun is
					when NOUN_MODULE => module_name_missing;
					when others => null; -- CS
				end case;
				
				
			when VERB_DELETE =>
				case noun is
					when NOUN_UNIT =>
						case cmd_field_count is
							when 4 =>
								device_name_missing;
								
							when 5 => -- like "delete unit IC1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then

									unit_delete.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_delete (
										device			=> unit_delete.device,
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;
								
							when 6 => -- like "delete unit IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then
									
									unit_delete.device := device_name;

									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed on the current active sheet.
									-- Deleting is possible if it is deployed and if it is on the current sheet.
									if deployed (current_active_module, unit_delete.device, unit_name) then

										unit_delete.unit := unit_name;
										
										if sheet (current_active_module, unit_delete.device, unit_delete.unit) = current_active_sheet then

											delete_unit (
												module_cursor	=> current_active_module,
												device_name		=> unit_delete.device,
												unit_name		=> unit_delete.unit,
												log_threshold	=> log_threshold + 1);
											
											-- CS redraw;

										else
											unit_not_on_this_sheet;
										end if;
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;
						end case;

					when others => null; -- CS
				end case;

				
			when VERB_DRAG =>
				case noun is
					when NOUN_UNIT =>
						case cmd_field_count is
							when 4 =>
								device_name_missing;
								
							when 5 => -- like "drag unit IC1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then
									unit_move.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_move (
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;
								
							when 6 => -- like "drag unit IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then
									
									unit_move.device := device_name;

									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed on the current active sheet.
									-- Dragging is possible if it is deployed and if it is on the current sheet.
									-- It will then be attached to the cursor or mouse pointer.
									if deployed (current_active_module, unit_move.device, unit_name) then

										unit_move.unit := unit_name;
										
										if sheet (current_active_module, unit_move.device, unit_move.unit) = current_active_sheet then
											select_unit_for_move;
											
											-- use the current primary tool for moving the unit:
											unit_move.tool := primary_tool;

											find_attached_segments;
											
											-- Allow drawing the unit:
											unit_move.being_moved := true;

											single_cmd_status.finalization_pending := true;
											-- CS redraw;

										else
											unit_not_on_this_sheet;
										end if;
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;								
						end case;

					when others => null; -- CS
				end case;

				
			when VERB_DRAW =>
				case noun is
					when NOUN_NET =>
						case cmd_field_count is
							when 4 =>
								-- no net name given -> anonymous net will be drawn
								set_status (et_canvas_schematic_nets.status_draw_net);
								single_cmd_status.finalization_pending := true;
								
							when 5 => -- like "draw net RESET_N"
								-- explicit net name given
								check_net_name_length (f (5));
								check_net_name_characters (to_net_name (f (5)));
								et_canvas_schematic_nets.preliminary_segment.net_name := to_net_name (f (5));

								set_status (et_canvas_schematic_nets.status_draw_net);
								single_cmd_status.finalization_pending := true;

							when others => null;								
						end case;
						
					when others => null;
				end case;

				
			when VERB_FETCH =>
				case noun is
					when NOUN_UNIT =>
						fetch_unit;

					when others => null; -- CS
				end case;
						
			when VERB_MOVE =>
				case noun is
					when NOUN_UNIT =>
						case cmd_field_count is
							when 4 =>
								device_name_missing;
								
							when 5 => -- like "move unit IC1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then
									unit_move.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_move (
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;
								
							when 6 => -- like "move unit IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then
									
									unit_move.device := device_name;

									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed.
									-- If it is deployed somewhere (whatever sheet) then it will be 
									-- attached to the cursor or mouse pointer.
									if deployed (current_active_module, unit_move.device, unit_name) then

										unit_move.unit := unit_name;
										
										-- If the unit is not on the current_active_sheet then notify the
										-- GUI that the sheet changes. This way the unit is drawn
										-- on the current visible sheet independed of its original sheet number.
										-- See et_canvas_schematic.draw_units.
										if sheet (current_active_module, unit_move.device, unit_move.unit) /= current_active_sheet then
											unit_move.sheet_changes := true;

											--set_status ("Moving unit from another sheet");
										end if;

										select_unit_for_move;
										
										-- use the current primary tool for moving the unit:
										unit_move.tool := primary_tool;
									
										-- Allow drawing the unit:
										unit_move.being_moved := true;

										single_cmd_status.finalization_pending := true;
										-- CS redraw;
										
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;								
						end case;

					-- moving placeholders for unit name, purpose and value:
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
						case cmd_field_count is
							when 4 => -- like "move name"
								device_name_missing;
								
							when 5 => -- like "move name R1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then

									placeholder_move.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_move (
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;

							when 6 => -- like "move name IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then

									placeholder_move.device := device_name;
									
									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed on the current active sheet.
									-- Moving the placeholder is possible if the unit it is deployed 
									-- and if it is on the current sheet.
									-- The placeholder will then be attached to the cursor or mouse pointer.
									if deployed (current_active_module, placeholder_move.device, unit_name) then

										placeholder_move.unit := unit_name;
										
										if sheet (current_active_module, placeholder_move.device, placeholder_move.unit) = current_active_sheet then
											finish_placeholder_move;
										else
											unit_not_on_this_sheet;
										end if;
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;								

								
						end case;
						
					when others => null; -- CS
				end case;

			when VERB_ROTATE =>
				case noun is
					when NOUN_UNIT =>
						case cmd_field_count is
							when 4 => device_name_missing;
								
							when 5 => -- like "rotate unit IC1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then
									unit_move.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_move (
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;
								
							when 6 => -- like "rotate unit IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then
									
									unit_move.device := device_name;

									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed on the current active sheet.
									-- Rotating is possible if it is deployed and if it is on the current sheet.
									-- It will then be attached to the cursor or mouse pointer.
									if deployed (current_active_module, unit_move.device, unit_name) then

										unit_move.unit := unit_name;
										
										if sheet (current_active_module, unit_move.device, unit_move.unit) = current_active_sheet then
											finish_unit_move;
										else
											unit_not_on_this_sheet;
										end if;
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;								
						end case;

					-- rotating placeholders for unit name, purpose and value:
					when NOUN_NAME | NOUN_PURPOSE | NOUN_VALUE =>
						case cmd_field_count is
							when 4 => -- like "rotate name"
								device_name_missing;
								
							when 5 => -- like "rotate name R1"
								unit_name_missing;

								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then

									placeholder_move.device := device_name;

									-- Propose units that are on the current active sheet:
									menu_propose_units_on_move (
										units			=> units_on_sheet (
															current_active_module,
															device_name,
															current_active_sheet,
															log_threshold + 1),
										log_threshold	=> log_threshold + 1);

								else
									device_not_found;
								end if;

							when 6 => -- like "rotate name IC1 B"
								device_name := to_device_name (f (5));
								
								if exists (current_active_module, device_name) then

									placeholder_move.device := device_name;
									
									unit_name := to_unit_name (f (6));

									-- Test whether the unit is deployed on the current active sheet.
									-- Rotating the placeholder is possible if the unit it is deployed 
									-- and if it is on the current sheet.
									-- The placeholder will then be attached to the cursor or mouse pointer.
									if deployed (current_active_module, placeholder_move.device, unit_name) then

										placeholder_move.unit := unit_name;
										
										if sheet (current_active_module, placeholder_move.device, placeholder_move.unit) = current_active_sheet then
											finish_placeholder_move;
										else
											unit_not_on_this_sheet;
										end if;
									else
										unit_not_deployed;
									end if; 
								else
									device_not_found;
								end if;
													
							when others => null;								

								
						end case;
						
					when others => null; -- CS
				end case;

			when VERB_SET =>
				case noun is
					when NOUN_PARTCODE | NOUN_PURPOSE | NOUN_VALUE =>
						case cmd_field_count is
							when 4 => device_name_missing;
								
							when 5 => -- like "set value/partcode/purpose IC1"
								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then
									set_property (device_name);
								else
									device_not_found;
								end if;

							when others => null;
						end case;

					when NOUN_VARIANT =>
						case cmd_field_count is
							when 4 => device_name_missing;
								
							when 5 => -- like "set variant IC1"
								device_name := to_device_name (f (5));

								if exists (current_active_module, device_name) then
									set_variant (device_name);
								else
									device_not_found;
								end if;

							when others => null;
						end case;

					when others => null; -- CS
				end case;

			when VERB_SHOW =>
				case noun is
					when NOUN_DEVICE =>
						case cmd_field_count is
							when 4 =>
								-- request operator to click on a unit:
								set_status (status_show_device);
								
							when others => null;
						end case;

					when NOUN_MODULE =>
						case cmd_field_count is
							when 4 => module_name_missing;
							-- CS request operator to click on the module.
							when others => null;
						end case;

					when NOUN_NET =>
						case cmd_field_count is
							when 4 =>
								-- request operator to click on a net:
								set_status (status_show_net);

							when others => null;
						end case;
						
					when NOUN_SHEET =>
						case cmd_field_count is
							when 4 => sheet_number_missing;
							when others => null;
						end case;

					when others => null;
				end case;
				
			when others => null; -- CS error message in status bar for other 
								-- incomplete commands such as zoom, position, ...
		
		end case;
	end propose_arguments;

	
begin -- schematic_cmd
	log (text => "given command: " 
		 & enclose_in_quotes (to_string (cmd_in)),
		 level => log_threshold);

	-- Make a copy of the given command. In case the given command is incomplete
	-- AND we are in graphical mode (non-headless) then
	-- this procedure interactively proposes arguments and completes the command.
	single_cmd_status := (cmd => cmd_in, others => <>);

	-- single_cmd_status.cmd will now be processed and interactively completed

	cmd_field_count := get_field_count (single_cmd_status.cmd);
	

	module := to_module_name (f (2)); -- motor_driver (without extension *.mod)
	-- CS: Becomes obsolete once all board ops use the
	-- given module_cursor.

	
	-- read the verb from field 3
	verb := to_verb (f (3));

	-- There are some very short commands which do not require a verb.
	-- For such commands we do not read the noun.
	case verb is
		when VERB_EXIT | VERB_QUIT => null; -- no noun
		when others => noun := to_noun (f (4)); -- read noun from field 4
	end case;

	-- Parse the command:
	parse;
	
	-- In case parse throws an exception, then the following statements 
	-- will be skipped.
	
	-- In graphical mode and cmd_entry_mode SINGLE_CMD the flag
	-- single_cmd_status.complete can change to false. In that case
	-- the interactive completiton starts here. 
	if not single_cmd_status.complete then
		propose_arguments;
	end if;

	-- After every command (regardless if it is complete or not)
	-- set the focus to the canvas:
	-- CS: remove ?
	-- if runmode /= MODE_HEADLESS then
	-- 	canvas.grab_focus; -- NOTE ! calls "cb_draw"
	-- end if;
	
	exception when event: others =>

			-- CS
			-- evaluate_exception (
			-- 	name	=> exception_name (event),
			-- 	message	=> exception_message (event));

			raise;
			
end schematic_cmd;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
