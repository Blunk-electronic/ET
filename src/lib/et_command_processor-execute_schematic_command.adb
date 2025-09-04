------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        EXECUTE SCHEMATIC COMMNAD                         --
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

with ada.containers;

with et_display;					use et_display;
with et_display.schematic;
with et_modes.schematic;
with et_canvas_schematic_units;
with et_canvas_schematic_nets;
with et_scripting_interactive_schematic;
with et_symbols;
with et_schematic_ops.netlists;
with et_schematic_ops.grid;
with et_schematic_ops.submodules;
with et_board_ops.grid;

with et_schematic_coordinates;
with et_module_instance;				use et_module_instance;
with et_unit_name;						use et_unit_name;
with et_units;
with et_sheets;							use et_sheets;
with et_net_labels;						use et_net_labels;
with et_net_connectors;					use et_net_connectors;
with et_net_strands;					use et_net_strands;
with et_nets;							use et_nets;
with et_net_names;						use et_net_names;
with et_net_class;						use et_net_class;
with et_schematic_text;					use et_schematic_text;
with et_schematic_ops.nets;
with et_schematic_ops.units;
with et_submodules;
with et_assembly_variants;
with et_assembly_variant_name;			use et_assembly_variant_name;
-- with et_pick_and_place;
with et_netlists;
with et_device_name;
with et_devices_electrical;
with et_device_library;					use et_device_library;
with et_device_placeholders;			use et_device_placeholders;

with et_device_purpose;					use et_device_purpose;
with et_device_partcode;				use et_device_partcode;
with et_device_model_names;
with et_device_value;					use et_device_value;
with et_package_variant;
with et_canvas.cmd;

with et_canvas_schematic_2;
with et_canvas_board_2;

with et_canvas_schematic_preliminary_object;	use et_canvas_schematic_preliminary_object;


separate (et_command_processor)



procedure execute_schematic_command (
	module_cursor	: in pac_generic_modules.cursor;
	cmd				: in out type_single_cmd;
	log_threshold	: in type_log_level)
is
	use pac_net_name;
	
	use et_project;
	use et_schematic_ops;
	use et_schematic_ops.nets;
	use et_schematic_ops.units;
	use et_schematic_ops.netlists;

	use et_schematic_coordinates;
	use pac_geometry_2;

	use pac_text_schematic;
	use et_device_name;
	use et_canvas_schematic_2;
	use et_canvas_schematic_2.pac_canvas;
	use et_display.schematic;
	use et_modes.schematic;

	package pac_canvas_cmd is new et_canvas_schematic_2.pac_canvas.cmd;
	use pac_canvas_cmd;

	

	-- This function is a shortcut to get a single field
	-- from the given command:
	function get_field (place : in type_field_count) 
		return string 
	is begin
		return get_field (cmd, place);
	end;

	
	-- This procedure sets the verb and the noun:
	procedure set_verb_and_noun is begin
		-- Set the verb.
		-- Read it from field 3:
		verb := to_verb (get_field (3));

		
		-- There are some very short commands which do not require a noun.
		-- For such commands we do not read the noun.
		case verb is
			when VERB_EXIT | VERB_QUIT => null; -- no noun
			
			-- Set the noun. Read it from field 4:		
			when others => noun := to_noun (get_field (4));
		end case;
	end set_verb_and_noun;



	
	-- Updates the verb-noun display depending on the 
	-- origin of the command and the runmode:
	procedure update_verb_noun_display is begin
		case get_origin (cmd) is
			when ORIGIN_CONSOLE => update_mode_display;

			when ORIGIN_SCRIPT =>
				-- put_line ("script");
			
				if runmode = MODE_MODULE then
					-- put_line ("module");
					-- log (text => "update verb-noun-display", level => log_threshold + 1);
					update_mode_display;
				end if;

		end case;
	end update_verb_noun_display;
	

	
	module	: pac_module_name.bounded_string; -- motor_driver (without extension *.mod)

	-- In order to tell the command processor that an operation is meant to 
	-- apply to the current sheet, we use the UNIX-bash-like period character:
	here : constant string := ".";


	-- Contains the number of fields given by the caller of this procedure:
	cmd_field_count : constant type_field_count := get_field_count (cmd);


	-- This procedure is a shortcut. Call it in case the given command is too long:
	procedure too_long is begin
		command_too_long (cmd, cmd_field_count - 1);
	end;


	-- This procedure is a shortcut. 
	-- Call it in case the given command is incomplete:
	procedure command_incomplete is begin
		command_incomplete (cmd);
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

	


	-- This procedure parses a command that set the 
	-- grid spacing like "schematic demo set grid spacing 20 20":
	procedure set_grid is 
		use et_schematic_ops.grid;
	begin
		-- Set the grid on the canvas:
		parse_canvas_command (cmd, VERB_SET, NOUN_GRID);

		-- The global variable "grid" has now been set
		-- as requested by the operator.
		
		-- Assign the grid in the database:
		set_grid (
			module_name 	=> module,
			grid			=> pac_canvas.grid,
			log_threshold	=> log_threshold + 1);

	end set_grid;
	


	procedure set_scale is begin

		parse_canvas_command (cmd, VERB_SET, NOUN_SCALE);
		
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
					variant_name	=> to_variant (get_field (5)),
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
					variant_name	=> to_variant (get_field (5)),
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
					variant_name	=> to_variant (get_field (5)), -- low_cost
					description		=> et_assembly_variants.to_unbounded_string (get_field (6)), -- "the cheap version"
					log_threshold	=> log_threshold + 1);
				
			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end describe_assembly_variant;


	

	procedure add_device is
		use et_device_model_names;
		use et_package_variant;
	begin
		case cmd_field_count is
			when 9 =>
				-- If a virtual device is added, then no variant is required.
				add_device (
					module_name 	=> module,
					device_model	=> to_file_name (get_field (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (6)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (7)),
									y => to_distance (get_field (8))
									)),
						rotation => to_rotation (get_field (9))
						),
					variant			=> to_variant_name (""),
					log_threshold	=> log_threshold + 1
					);

			when 10 =>
				-- A real device requires specification of a package variant.
				add_device (
					module_name 	=> module,
					device_model	=> to_file_name (get_field (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (6)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (7)),
									y => to_distance (get_field (8))
									)),
						rotation		=> to_rotation (get_field (9))
						),
					variant			=> to_variant_name (get_field (10)),
					log_threshold	=> log_threshold + 1
					);

			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;

	end add_device;


	

	-- This procedure parses a command that renames a device like
	-- "schematic led_driver rename device IC1 IC2"
	procedure rename_device is 
	begin
		case cmd_field_count is
			when 6 =>
				rename_device (
					module_cursor 		=> active_module,
					device_name_before	=> to_device_name (get_field (5)), -- IC1
					device_name_after	=> to_device_name (get_field (6)), -- IC23
					log_threshold		=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case; 
	end rename_device;


	
	
	
	procedure add_netchanger is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 8 =>
				add_netchanger (
					module_name 	=> module,
					place			=> to_position 
						(
						sheet => to_sheet (get_field (5)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (6)),
									y => to_distance (get_field (7))
									)),
						rotation		=> to_rotation (get_field (8))
						),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end add_netchanger;
	

	

	procedure move_netchanger is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				move_netchanger
					(
					module_name 	=> module,
					index			=> et_submodules.to_netchanger_id (get_field (5)), -- 1,2,3, ...
					coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
					sheet			=> to_sheet_relative (get_field (7)),
					point			=> type_vector_model (set (
										x => to_distance (get_field (8)),
										y => to_distance (get_field (9)))),
						
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
					index			=> et_submodules.to_netchanger_id (get_field (5)), -- 1,2,3,...
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
					index			=> et_submodules.to_netchanger_id (get_field (5)), -- 1,2,3,...
					coordinates		=> to_coordinates (get_field (6)), -- relative/absolute
					point			=> type_vector_model (set (
										x => to_distance (get_field (7)),
										y => to_distance (get_field (8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end drag_netchanger;



	
	
	procedure rotate_netchanger is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 7 =>
				rotate_netchanger (
					module_name 	=> module,
					index			=> et_submodules.to_netchanger_id (get_field (5)), -- 1,2,3,...
					coordinates		=> to_coordinates (get_field (6)), -- relative/absolute
					rotation		=> to_rotation (get_field (7)), -- 90
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
					instance		=> to_instance_name (get_field (5)),
					port_name		=> to_net_name (get_field (6)),
					position		=> type_vector_model (set 
								(
								x => to_distance (get_field (7)),
								y => to_distance (get_field (8))
								)),
					direction		=> et_submodules.to_port_name (get_field (9)),
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
					instance		=> to_instance_name (get_field (5)),
					port_name		=> to_net_name (get_field (6)),
					coordinates		=> to_coordinates (get_field (7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (8)),
								y => to_distance (get_field (9)))),
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
					instance		=> to_instance_name (get_field (5)),
					port_name		=> to_net_name (get_field (6)),
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
					instance		=> to_instance_name (get_field (5)),
					port_name		=> to_net_name (get_field (6)),
					coordinates		=> to_coordinates (get_field (7)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (8)),
								y => to_distance (get_field (9)))),
					log_threshold	=> log_threshold + 1
					);

			when 10 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end move_port_of_submodule;
	
	


	
	procedure add_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 11 =>
				add_submodule (
					module_name 	=> module, -- parent module (where the submodule is to be inserted)
					file			=> et_submodules.to_submodule_path (get_field (5)),
					instance		=> to_instance_name (get_field (6)), -- submodule instance name
					position		=> to_position 
						(
						sheet => to_sheet (get_field (7)),
						point => type_vector_model (set 
									(
									x => to_distance (get_field (8)),
									y => to_distance (get_field (9))
									))
						),
					size => (
						x => to_distance (get_field (10)),
						y => to_distance (get_field (11))
						),
					log_threshold	=> log_threshold + 1
					);

			when 12 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end add_submodule;




	
	procedure move_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				move_submodule (
					module_name 	=> module,
					instance		=> to_instance_name (get_field (5)),
					coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
					sheet			=> to_sheet_relative (get_field (7)),
					point			=> type_vector_model (set (
								x => to_distance (get_field (8)),
								y => to_distance (get_field (9)))),
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
					instance		=> to_instance_name (get_field (5)),
					coordinates		=> to_coordinates (get_field (6)),  -- relative/absolute
					point			=> type_vector_model (set (
								x => to_distance (get_field (7)),
								y => to_distance (get_field (8)))),
					log_threshold	=> log_threshold + 1
					);

			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end drag_submodule;
	




	
	procedure copy_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 9 =>
				copy_submodule (
					module_name 	=> module, -- parent module (where the submodule is to be copied)
					instance_origin	=> to_instance_name (get_field (5)), -- submodule instance name
					instance_new	=> to_instance_name (get_field (6)), -- submodule instance name
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (7)),
						point => type_vector_model (set
									(
									x => to_distance (get_field (8)),
									y => to_distance (get_field (9))
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
					instance		=> to_instance_name (get_field (5)), -- submodule instance name
					log_threshold	=> log_threshold + 1
					);

			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_submodule;


	
	

	procedure rename_submodule is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 6 =>
				rename_submodule
					(
					module_name		=> module,
					instance_old	=> to_instance_name (get_field (5)), -- OSC1
					instance_new	=> to_instance_name (get_field (6)), -- OSC2
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end rename_submodule;



	

	procedure mount_submodule is
		use et_schematic_ops.submodules;
		use et_assembly_variants;
	begin
		case cmd_field_count is
			when 7 =>
				mount_submodule
					(
					module_name		=> module,
					variant_parent	=> to_variant (get_field (5)), -- low_cost
					instance		=> to_instance_name (get_field (6)), -- OSC1
					variant_submod	=> to_variant (get_field (7)), -- fixed_frequency
					log_threshold	=> log_threshold + 1);

			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;

		end case;
	end mount_submodule;
	
	

	

	procedure remove_submodule is
		use et_schematic_ops.submodules;
		use et_assembly_variants;
	begin
		case cmd_field_count is
			when 6 =>
				remove_submodule
					(
					module_name		=> module,
					variant_parent	=> to_variant (get_field (5)),
					instance		=> to_instance_name (get_field (6)), -- OSC1
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end remove_submodule;


	

	
	procedure set_submodule_file is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 6 =>
				set_submodule_file (
					module_name 	=> module,
					instance		=> to_instance_name (get_field (5)),
					file			=> et_submodules.to_submodule_path (get_field (6)),
					log_threshold	=> log_threshold + 1
					);

			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end set_submodule_file;



	
	
	procedure build_submodules_tree is
		use et_schematic_ops.submodules;
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
		use et_schematic_ops.submodules;
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



	

	procedure make_boms is
		use et_schematic_ops.submodules;
	begin
		case cmd_field_count is
			when 4 =>
				make_boms -- a BOM for each variant
					(
					module_name 	=> module,
					log_threshold	=> log_threshold + 1);

			when 5 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end make_boms;
		

	

	procedure copy_device is
	begin
		case cmd_field_count is
			when 9 =>
				copy_device (
					module_name 	=> module,
					device_name		=> to_device_name (get_field (5)),
					destination		=> to_position 
						(
						sheet => to_sheet (get_field (6)),
						point => type_vector_model (set
									(
									x => to_distance (get_field (7)),
									y => to_distance (get_field (8))
									)),
						rotation		=> to_rotation (get_field (9))
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
		device	: in type_device_name; -- IC45
		unit	: in pac_unit_name.bounded_string := to_unit_name (""); -- A, B, ..
		mode	: in type_show_device := FIRST_UNIT)
	is
		use pac_unit_name;
		use et_units;
		use et_devices_electrical;
		use et_canvas_schematic_2;

		
		function locate (unit : in pac_unit_name.bounded_string) 
			return type_unit_query
		is begin
			return get_unit_position (
				module_cursor	=> active_module,
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
						active_sheet := get_sheet (location.position);

						-- center on the first unit
					-- CS center_on (canvas, location.position.place);

						-- Make the whole device (with all its units) selected:
						proposed_units.append (new_item => (
							device	=> locate_device (active_module, device),
							unit	=> pac_units.no_element));

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
						active_sheet := get_sheet (location.position);

						-- center on the unit
					-- CS center_on (canvas, location.position.place);

						-- Make the whole device (with all its units) selected:
						proposed_units.append (new_item => (
							device	=> locate_device (active_module, device),
							unit	=> locate_unit (active_module, device, unit)));

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
				begin
					if location.exists then
						if get_sheet (location.position) = active_sheet then

							-- center on the unit
						-- CS	center_on (canvas, location.position.place);

							-- Make the whole device (with all its units) selected:
							proposed_units.append (new_item => (
								device	=> locate_device (active_module, device),
								unit	=> pac_units.no_element));

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



	
	-- This procedure parses a command that deletes a 
	-- whole device (with all units) like
	-- "schematic led_driver delete unit IC1":
	procedure delete_device is 
		device_name : type_device_name;
	begin
		device_name := to_device_name (get_field (5));
		
		case cmd_field_count is
			when 5 =>
				delete_device (
					module_cursor 	=> active_module,
					device_name		=> device_name,
					log_threshold	=> log_threshold + 1);

			when 6 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_device;

	

	

	-- This procedure parses a command that deletes a unit
	-- of a device like "schematic led_driver delete unit IC1 C":
	procedure delete_unit is 
		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
	begin
		device_name := to_device_name (get_field (5));
		unit_name	:= to_unit_name (get_field (6));
		
		case cmd_field_count is
			when 6 =>
				delete_unit (
					module_cursor 	=> active_module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					log_threshold	=> log_threshold + 1);

			when 7 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_unit;
	



	
	procedure drag_unit is begin
		case cmd_field_count is
			when 9 =>
				drag_unit (
					module_cursor 	=> active_module,
					device_name		=> to_device_name (get_field (5)),
					unit_name		=> to_unit_name (get_field (6)),
					coordinates		=> to_coordinates (get_field (7)), -- relative/absolute
					destination		=> type_vector_model (set (
										x => to_distance (get_field (8)),
										y => to_distance (get_field (9)))),
					log_threshold	=> log_threshold + 1);

			when 10 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end drag_unit;
	



	-- Parses a command that moves a unit either relatively or
	-- absolutely:
	-- example 1: schematic led_driver move unit IC1 A relative -1 2 4
	-- example 2: schematic led_driver move unit IC1 C absolute 2 210 100
	procedure move_unit is 
		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
		coordinates : type_coordinates;
		sheet		: type_sheet_relative;		
		destination	: type_vector_model;
	begin
		device_name := to_device_name (get_field (5)); -- IC1
		unit_name	:= to_unit_name (get_field (6)); -- A
		coordinates := to_coordinates (get_field (7)); -- relative/absolute
		sheet		:= to_sheet_relative (get_field (8)); -- -1, 2

		destination	:= set (x => to_distance (get_field (9)), -- 2, 210
							y => to_distance (get_field (10))); -- 4, 100

		case cmd_field_count is
			when 10 =>
				move_unit (
					module_cursor 	=> active_module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					coordinates		=> coordinates,
					sheet			=> sheet,
					destination		=> destination,						
					log_threshold	=> log_threshold + 1);

			when 11 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end move_unit;
	

	
	
	-- Parses a command that rotates a unit either relatively or
	-- absolutely:
	-- example 1: schematic led_driver rotate unit IC1 A relative -90
	-- example 2: schematic led_driver rotate unit IC1 B absolute 90
	procedure rotate_unit is begin
		case cmd_field_count is
			when 8 =>
				rotate_unit (
					module_cursor 	=> active_module,
					device_name		=> to_device_name (get_field (5)), -- IC1
					unit_name		=> to_unit_name (get_field (6)), -- A
					coordinates		=> to_coordinates (get_field (7)),  -- relative/absolute
					rotation		=> to_rotation (get_field (8)), -- 90
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end rotate_unit;
		
	


	-- Parses a command that fetches a unit from a device
	-- and places it in the schematic:
	-- example: 
	-- "schematic demo fetch unit IC1 C 1 70 100 -90"
	procedure fetch_unit is
		device_name : type_device_name;
		unit_name	: pac_unit_name.bounded_string;
		sheet		: type_sheet;
		place		: type_vector_model;
		rotation	: type_rotation;
	begin
		device_name := to_device_name (get_field (5)); -- IC1
		unit_name	:= to_unit_name (get_field (6)); -- C
		sheet		:= to_sheet (get_field (7)); -- 1
		place		:= set (x => to_distance (get_field (8)), -- 70
							y => to_distance (get_field (9))); -- 100
	
		rotation	:= to_rotation (get_field (10)); -- -90

		case cmd_field_count is
			when 10 =>
				fetch_unit (
					module_cursor	=> active_module,
					device_name		=> device_name,
					unit_name		=> unit_name,
					destination		=> to_position (place, sheet, rotation),
					log_threshold	=> log_threshold + 1);

			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end fetch_unit;
	

	


	procedure rotate_unit_placeholder is
		meaning : type_placeholder_meaning;

		procedure do_it is begin
			case cmd_field_count is
				when 7 =>
					rotate_unit_placeholder (
						module_cursor 	=> active_module,
						device_name		=> to_device_name (get_field (5)), -- IC1
						unit_name		=> to_unit_name (get_field (6)), -- A
						rotation		=> to_rotation_doc (get_field (7)), -- 90
						meaning			=> meaning,
						log_threshold	=> log_threshold + 1);

				when 8 .. type_field_count'last => too_long; 
					
				when others => command_incomplete;
			end case;
		end do_it;
		
		
	begin
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end rotate_unit_placeholder;
	


	

	procedure move_unit_placeholder is
		meaning : type_placeholder_meaning;

		procedure do_it is begin
			-- schematic led_driver move name R1 1 absolute 10 15
			case cmd_field_count is
				when 9 =>
					move_unit_placeholder (
						module_cursor 	=> active_module,
						device_name		=> to_device_name (get_field (5)), -- IC1
						unit_name		=> to_unit_name (get_field (6)), -- A
						coordinates		=> to_coordinates (get_field (7)),  -- relative/absolute
						point			=> type_vector_model (set (
											x => to_distance (get_field (8)),
											y => to_distance (get_field (9)))),
						meaning			=> meaning,
						log_threshold	=> log_threshold + 1);

				when 10 .. type_field_count'last => too_long; 
					
				when others => command_incomplete;
			end case;
		end do_it;

		
	begin
		case noun is
			when NOUN_NAME =>
				meaning := NAME;
				
			when NOUN_VALUE =>
				meaning := VALUE;
								
			when NOUN_PURPOSE =>
				meaning := PURPOSE;

			-- CS partcode ?

			when others => null; -- CS should never happen
		end case;

		do_it;		
	end move_unit_placeholder;
	

	
	
	


	-- This procedure parses a command that highlights a net:
	procedure show_net is
		use pac_nets;
		use et_canvas_schematic_nets;

		net_name : pac_net_name.bounded_string; -- RESET_N
		net_cursor : pac_nets.cursor;

	begin
		case cmd_field_count is
			when 5 => 
				net_name := to_net_name (get_field (5)); -- RESET_N

				net_cursor := locate_net (active_module, net_name);

				if has_element (net_cursor) then				
					reset_nets (active_module, log_threshold + 1);
					show_net (active_module, net_cursor, log_threshold + 1);
				else
					log (WARNING, "Net " & to_string (net_name) & " does not exist !");
					-- CS set_status ?
				end if;
			
			
			when 6 .. type_field_count'last => too_long;
			
			when others => command_incomplete;
		end case;

		-- CS:
		-- "ERROR: Net " & enclose_in_quotes (to_string (net))
		-- 	& " is not on this sheet !";
   		

	end show_net;




	-- This procedure parses a command that places a net connector.
	-- Example: "schematic demo place net_connector 1 60 80 input"
	procedure place_net_connector is begin
		case cmd_field_count is
			when 8 =>

				place_net_connector (
					module_cursor	=> active_module,
					position		=> to_position (
											point => type_vector_model (set (
												x => to_distance (get_field (6)),
												y => to_distance (get_field (7)))),
											sheet => to_sheet (get_field (5))), -- sheet number
    
					-- A connector requires specification of signal direction:
					direction		=> to_direction (get_field (8)), -- INPUT, OUTPUT, PASSIVE, ...
					log_threshold	=> log_threshold + 1);

				
			when 9 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end place_net_connector;


	
	
	-- This procedure parses a command that places a net connector.
	-- Example: "schematic demo place net_label 1 70 80"
	procedure place_net_label is begin
		case cmd_field_count is
			when 7 =>
				
				place_net_label (
					module_cursor	=> active_module,
					position		=> to_position (
											point => type_vector_model (set (
												x => to_distance (get_field (6)),
												y => to_distance (get_field (7)))),
											sheet => to_sheet (get_field (5))), -- sheet number
    
					log_threshold	=> log_threshold + 1);

				
			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end place_net_label;




	

	

	-- Parses a command like:
	-- "schematic demo draw net RESET_N 1 90 100  100 100"
	-- The command can be shorter than the above example, because
	-- the operator is not required to type everything:
	procedure draw_net is
		use et_canvas_schematic_nets;
		
		procedure no_name_given is begin
			-- If this is the first net right after system start,
			-- then an anonymous net will be used.
			-- Otherwise the net name used last will be applied
			-- as it is stored in object_net_name.
			if is_empty (object_net_name) then -- after system start
				object_net_name := get_lowest_available_anonymous_net (module_cursor);
				log (text => "apply anonymous name: " & to_string (object_net_name),
					 level => log_threshold + 2);
			else
				log (text => "apply name used last: " & to_string (object_net_name),
					 level => log_threshold + 2);
			end if;

			set_status (status_draw_net & " of net " & to_string (object_net_name));
		end;

		
		procedure explicit_name_given is
			name_s : constant string := get_field (5); -- RESET_N
			name_b : pac_net_name.bounded_string;
		begin
			-- Validate the given net name:
			check_net_name_length (name_s);
			name_b := to_net_name (name_s);
			check_net_name_characters (name_b);

			-- Assign the net name:
			object_net_name := name_b;

			set_status (status_draw_net & " of net " & to_string (object_net_name));
		end explicit_name_given;


		
		procedure segment_given is
			name_s : constant string := get_field (5); -- RESET_N
			name_b : pac_net_name.bounded_string;

			A : type_object_position; -- start point of segment
			B : type_vector_model; -- end point of segment
		begin
			-- Validate the given net name:
			check_net_name_length (name_s);
			name_b := to_net_name (name_s);
			check_net_name_characters (name_b);

			-- Assign the net name:
			object_net_name := name_b;

			-- Assign the start and end of the segment:
			A := to_position (
				point => to_vector_model (get_field (7), get_field (8)), -- x/y
				sheet => to_sheet (get_field (6))); -- sheet number

			B := to_vector_model (get_field (9), get_field (10)); -- x/y

			-- Insert the net segment in the database:
			insert_net_segment (
				module_cursor	=> active_module,
				net_name		=> object_net_name,
				A				=> A,					
				B 				=> B,					
				log_threshold	=> log_threshold + 1);
		end segment_given;
		
		
	begin		
		log_indentation_up;

		case get_origin (cmd) is
			when ORIGIN_CONSOLE =>

				-- The command may contain more or less arguments
				-- and can still be valid.
				-- However a minimum of arguments must be ensured
				-- and a maximum must not be exceeded:
				case cmd_field_count is
					when 4 => -- like "draw net"
						log (text => "no name given", level => log_threshold + 1);
						log_indentation_up;
						no_name_given;
						log_indentation_down;
						
					when 5 => -- like "draw net RESET_N"
						explicit_name_given;
					
					when 10 => -- like "draw net RESET_N 1 90 100  100 100"
						segment_given;

					when 11 .. type_field_count'last => 
						too_long; 
					
					when others =>
						command_incomplete;
				end case;

				
				
			when ORIGIN_SCRIPT =>

				-- The command MUST contain a certain number of
				-- arguments:
				case cmd_field_count is
					when 10 => -- like "draw net RESET_N 1 90 100  100 100"
						segment_given;

					when 11 .. type_field_count'last => 
						too_long; 
						
					when others => 
						command_incomplete;
						
				end case;

		end case;
		
		log_indentation_down;

		-- CS exception handler
		-- CS set_exit_code (cmd, 3);
	end draw_net;




	-- This procedure parses a command that deletes a net connector:
	procedure delete_net_connector is begin
		null; -- CS
	end; 
	

	
	-- This procedure parses a command that deletes a net label:
	procedure delete_net_label is begin
		case cmd_field_count is
			when 7 =>
				delete_net_label
					(
					module_cursor	=> active_module,

					position		=> to_position (
										point => type_vector_model (set (
											x => to_distance (get_field (6)),
											y => to_distance (get_field (7)))),
										sheet => to_sheet (get_field (5))), -- sheet number
					
					log_threshold	=> log_threshold + 1);
				
			when 8 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_net_label;



	-- This procedure parses a command that moves a net label:
	procedure move_net_label is begin
		null;
		-- CS
	end move_net_label;

	

	procedure delete_net is
	begin
		case cmd_field_count is
			-- example 1: "delete net RESET_N"
			-- example 2: "delete net RESET_N 2"

			when 5 =>
				delete_net (
					module_cursor		=> active_module,
					net_name			=> to_net_name (get_field (5)), -- RESET_N
					sheet				=> 1, -- no meaning
					all_sheets			=> TRUE,
					log_threshold		=> log_threshold + 1);

			when 6 =>
				delete_net (
					module_cursor		=> active_module,
					net_name			=> to_net_name (get_field (5)), -- RESET
					sheet				=> to_sheet (get_field (6)),
					log_threshold		=> log_threshold + 1);
			
			when 7 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end delete_net;



	
	procedure delete_net_segment is
		catch_zone : type_catch_zone;
	begin
		case cmd_field_count is
			-- example: "delete segment 1 97 99 2"
			when 8 =>
				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (6), get_field (7)),
					radius	=> to_zone_radius (get_field (8)));
				
				delete_segment (
					module_cursor	=> active_module,
					sheet			=> to_sheet (get_field (5)),
					catch_zone		=> catch_zone,
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_net_segment;




	procedure delete_net_strand is
		catch_zone : type_catch_zone;
	begin
		case cmd_field_count is
			-- example: "delete strand 1 97 99 2"
			when 8 =>
				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (6), get_field (7)),
					radius	=> to_zone_radius (get_field (8)));
				
				delete_strand (
					module_cursor	=> active_module,
					sheet			=> to_sheet (get_field (5)),
					catch_zone		=> catch_zone,
					log_threshold	=> log_threshold + 1);

			when 9 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end delete_net_strand;


	
	

	procedure drag_net_segment is
		catch_zone : type_catch_zone;
	begin
		-- example: "drag segment 1 80 100 2 relative 10 0"
		case cmd_field_count is
			when 11 =>

				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (6), get_field (7)),
					radius	=> to_zone_radius (get_field (8)));
				
				drag_segment (
					module_cursor	=> active_module,
					sheet			=> to_sheet (get_field (5)),
					catch_zone		=> catch_zone,					
					coordinates		=> to_coordinates (get_field (9)), -- relative/absolute					
					destination		=> to_vector_model (get_field (10), get_field (11)),
					log_threshold	=> log_threshold + 1);
					

			when 13 .. type_field_count'last => too_long; 
				
			when others => command_incomplete;
		end case;
	end drag_net_segment;
	


	

	procedure rename_net is 
		catch_zone : type_catch_zone;
	begin
		case cmd_field_count is

			-- If the command has only 6 fields, then
			-- all strands on all sheets are renamed.
			-- example: rename net RESET_N RST_N
			when 6 =>
				-- Rename the net everywhere:
				rename_net (
					module_cursor		=> active_module,
					net_name_before		=> to_net_name (get_field (5)), -- RESET
					net_name_after		=> to_net_name (get_field (6)), -- RESET_N
					all_sheets			=> true,
					log_threshold		=> log_threshold + 1);

				
			-- If the command has 7 fields, then
			-- the renaming takes place on the strands of the given sheet only.	
			-- Sheet is set by the 7th argument.
			-- and are further-on ignored by the called procedure.
			-- example: rename net RESET_N RST_N 2
			when 7 =>
				-- Rename the net on the given sheet:
				rename_net (
					module_cursor		=> active_module,
					net_name_before		=> to_net_name (get_field (5)), -- RESET
					net_name_after		=> to_net_name (get_field (6)), -- RESET_N
					sheet				=> to_sheet (get_field (7)), -- 2
					log_threshold		=> log_threshold + 1);

				
			-- If the command has 10 fields, the net scope is STRAND.
			-- Place is set according to arguments 7..9.
			-- example: rename net RESET_N RST_N 2 50 90 5
			when 10 =>
				-- Rename a strand on a given sheet:

				catch_zone := set_catch_zone (
					center	=> to_vector_model (get_field (8), get_field (9)),
					radius	=> to_zone_radius (get_field (10))); -- 50 90 5

				rename_strand (
					module_cursor		=> active_module,
					net_name_before		=> to_net_name (get_field (5)), -- RESET
					net_name_after		=> to_net_name (get_field (6)), -- RESET_N
					sheet				=> to_sheet (get_field (7)), -- 2
					catch_zone			=> catch_zone,
					log_threshold		=> log_threshold + 1);
				
			when 11 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;
	end rename_net;


	
	
	-- This procedure extracts from the command the
	-- sheet number and sets it active.
	-- It updates the sheet number display accordingly:
	procedure show_sheet is -- GUI related
		sheet : type_sheet;

		procedure show is
		begin
			sheet := to_sheet (get_field (5));

			-- CS test whether sheet exists
			
			active_sheet := sheet;
			update_sheet_number_display;
		end show;
			
	begin
		log (text => "set sheet" & to_string (sheet), level => log_threshold + 1); 
		
		case cmd_field_count is
			when 5 => show;
			when 6 .. type_field_count'last => too_long;
			when others => command_incomplete;
		end case;
		
	end show_sheet;




	
	procedure create_module is
		
		procedure do_it (
			module_name : in pac_module_name.bounded_string) 
		is
			use pac_generic_modules;
		begin
			create_module (
				module_name		=> module_name, -- led_driver_test
				log_threshold	=> log_threshold + 1);

			-- Show the module in schematic and board editor:
			
			active_module := locate_module (module_name);
			active_sheet := 1;

			-- Update module name in the schematic window title bar:
			-- CS set_title_bar (active_module);
			
			-- CS update_sheet_number_display;
			
			-- Update the board window title bar:
			-- CS et_canvas_board_2.set_title_bar (active_module);
		end do_it;

		
	begin
		case cmd_field_count is
			when 5 => do_it (to_module_name (get_field (5)));
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

		sheet : type_sheet := 1;

		
		-- Sets the active module and first sheet.
		procedure module_and_first_sheet is begin
			module := to_module_name (get_field (5));
			set_module (module);
			active_sheet := sheet;
			
			update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_first_sheet;



		-- Sets the active module and sheet.
		procedure module_and_random_sheet is begin
			module := to_module_name (get_field (5));
			set_module (module);

			log (text => "sheet " & to_string (sheet), 
				level => log_threshold + 1);
			
			sheet := to_sheet (get_field (6));
			active_sheet := sheet;

			update_schematic_editor;
			et_canvas_board_2.update_board_editor;
		end module_and_random_sheet;
		
		
	begin
		log (text => "show module (via schematic editor) " 
			 & enclose_in_quotes (to_string (module)),
			 level => log_threshold + 1);

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
			when 5 => do_it (noun, get_field (5));
			when 6 .. type_field_count'last => too_long; 
			when others => command_incomplete;
		end case;
	end display;
	

	

	procedure delete_module is

		use ada.containers;
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
				
				active_module := generic_modules.first;
				active_sheet := 1;

				log (text => "set module " 
					 & enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board_2.set_title_bar (active_module);
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
			
				active_module := generic_modules.first;
				active_sheet := 1;

				log (text => "set module " 
					 & enclose_in_quotes (get_active_module), 
					level => log_threshold + 1);

				-- Update module name in the schematic window title bar:
				set_title_bar (active_module);
				
				update_sheet_number_display;
				
				-- Update the board window title bar:
				et_canvas_board_2.set_title_bar (active_module);
			else
				-- CS
				null;
				-- terminate_main;
			end if;
		end delete_explicit;

		
	begin
		case cmd_field_count is
			when 4 => delete_active;								
			when 5 => delete_explicit (to_module_name (get_field (5)));
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
					module_cursor	=> active_module,
					log_threshold	=> log_threshold + 1);

			when 5 =>
				-- Save the module with a different name:
				save_module (
					module_cursor	=> active_module,
					save_as_name	=> to_module_name (get_field (5)), -- led_driver_test
					log_threshold	=> log_threshold + 1);
				
			when 6 .. type_field_count'last => too_long;
				
			when others => command_incomplete;
		end case;			

	end save_module;

	
	




	

	
	-- Parses the given command and dispatches to
	-- further subroutines:
	procedure parse is 
		use et_device_placeholders;
		use et_assembly_variants;
	begin

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
						
					when NOUN_NET_CONNECTOR =>
						delete_net_connector;

					when NOUN_NET_LABEL =>
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
						
					when NOUN_STRAND =>
						delete_net_strand;

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
					when NOUN_NET => draw_net;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_EXECUTE =>
				case noun is
					when NOUN_SCRIPT =>
						parse_execute_script (cmd, log_threshold);
							
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
						parse_canvas_command (cmd, VERB_MOVE, NOUN_CURSOR);

						
					when NOUN_NAME | NOUN_VALUE | NOUN_PARTCODE | NOUN_PURPOSE =>
						move_unit_placeholder;

						
					when NOUN_PORT =>
						move_port_of_submodule;
						
					when NOUN_NETCHANGER =>
						move_netchanger;


					when NOUN_NET_LABEL =>
						move_net_label;
								
					when NOUN_TEXT =>
						NULL; -- CS

						
					when NOUN_SUBMODULE =>
						move_submodule;

						
					when NOUN_UNIT =>
						move_unit;
								
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_MAKE =>
				case noun is
					when NOUN_BOM => 
						make_boms;


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
							value := to_value_with_check (get_field (7));

							-- validate partcode
							partcode := to_partcode (get_field (8));
							
							case cmd_field_count is
								when 8 =>
									-- set value and partcode
									mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (get_field (5)), -- low_cost
										device			=> to_device_name (get_field (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										log_threshold	=> log_threshold + 1);

								when 9 =>
									-- optionally the purpose can be set also
									purpose := to_purpose (get_field (9)); -- brightness_control
												
									mount_device
										(
										module_name		=> module,
										variant_name	=> to_variant (get_field (5)), -- low_cost
										device			=> to_device_name (get_field (6)), -- R1
										value			=> value, -- 220R
										partcode		=> partcode, -- R_PAC_S_0805_VAL_220R
										purpose			=> purpose, -- brightness_control
										log_threshold	=> log_threshold + 1);
									
								when 10 .. type_field_count'last => too_long;
									
								when others => command_incomplete;
							end case;

						end; -- declare

						
					when NOUN_SUBMODULE =>
						mount_submodule;
											
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_PLACE =>
				case noun is
					when NOUN_NET_CONNECTOR =>
						place_net_connector;

					when NOUN_NET_LABEL =>
						place_net_label;

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
									variant_name	=> to_variant (get_field (5)), -- low_cost
									device			=> to_device_name (get_field (6)), -- R1
									log_threshold	=> log_threshold + 1);

							when 7 .. type_field_count'last => too_long;
								
							when others => command_incomplete;
						end case;

						
					when NOUN_SUBMODULE =>
						remove_submodule;
						
					when others => invalid_noun (to_string (noun));
				end case;

				
			when VERB_RENAME =>
				case noun is
					when NOUN_DEVICE =>
						rename_device;
						
					when NOUN_SUBMODULE =>
						rename_submodule;
						
					when NOUN_NET =>
						rename_net;
				
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
									step_width		=> to_index (get_field (5)), -- 100
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
						rotate_unit;
								
					when NOUN_NAME | NOUN_VALUE | NOUN_PURPOSE | NOUN_PARTCODE =>
						rotate_unit_placeholder;
						
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
									module_cursor	=> active_module,
									net_name		=> to_net_name (get_field (5)),
									net_class		=> to_net_class_name (get_field (6)),
									log_threshold	=> log_threshold + 1);
								
							when 7 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;
						
						
					when NOUN_GRID =>
						set_grid;

					when NOUN_CURSOR =>
						parse_canvas_command (cmd, VERB_SET, NOUN_CURSOR);

					when NOUN_ZOOM =>
						parse_canvas_command (cmd, VERB_SET, NOUN_ZOOM);
						
					when NOUN_SCALE =>
						set_scale;


						
					when NOUN_PARTCODE =>
						case cmd_field_count is
							when 6 =>
								declare
									partcode : pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
								begin
									partcode := to_partcode (get_field (6));

									-- set the purpose
									set_partcode
										(
										module_name 	=> module,
										device_name		=> to_device_name (get_field (5)), -- R1
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
									purpose : pac_device_purpose.bounded_string; -- brightness_control
								begin
									purpose := to_purpose (get_field (6));
									
									-- set the purpose
									set_purpose
										(
										module_name 	=> module,
										device_name		=> to_device_name (get_field (5)), -- R1
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
									module_cursor 	=> active_module,
									net_name		=> to_net_name (get_field (5)),
									scope			=> et_netlists.to_net_scope (get_field (6)),
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
									value := to_value_with_check (get_field (6));

									-- set the value
									set_value
										(
										module_name 	=> module,
										device_name		=> to_device_name (get_field (5)), -- R1
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
									use et_package_variant;
									variant : pac_package_variant_name.bounded_string; -- N, D
								begin
									-- validate variant
									check_variant_name_length (get_field (6));
									variant := to_variant_name (get_field (6));
									check_variant_name_characters (variant);
									
									-- set the variant
									set_variant
										(
										module			=> module,
										device			=> to_device_name (get_field (5)), -- IC1
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
									device	=> to_device_name (get_field (5)), -- R1, IC1
									mode	=> FIRST_UNIT);
							
							when 6 =>
								-- The 6th field may be a period, which means
								-- the unit is to be shown on the current active sheet.
								-- Otherwise the field provides an explicit
								-- unit name:
								if get_field (6) = here then
									show_device ( -- show device IC1 .
										device	=> to_device_name (get_field (5)), -- IC1
										mode	=> FIRST_UNIT_ON_CURRENT_SHEET);
								else
									show_device ( -- show device IC1 A
										device	=> to_device_name (get_field (5)), -- IC1
										unit	=> to_unit_name (get_field (6)), -- A
										mode	=> BY_UNIT_NAME);
								end if;
								
							when 7 .. type_field_count'last => too_long;
							when others => command_incomplete;
						end case;

					when NOUN_MODULE =>
						show_module;

						
					when NOUN_NET =>
						show_net;
						
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
									variant_name	=> to_variant (get_field (5)), -- low_cost
									device			=> to_device_name (get_field (6)), -- R1
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

		
		
		exception
			when event: others =>
				-- log (text => ada.exceptions.exception_information (event), console => true);
				log (text => ada.exceptions.exception_information (event));
		
	end parse;		




	

	-- This procedure proposes missing arguments:	
	procedure propose_arguments is
		use et_scripting_interactive_schematic;
		use et_canvas_schematic_units;
		use et_canvas_schematic_nets;

		device_name		: type_device_name;
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

		
		procedure unit_not_found is 
			use pac_unit_name;
		begin
			set_status ("ERROR: Device " & to_string (device_name) 
				& " does not provide unit " & to_string (unit_name) & " !");
		end unit_not_found;

		
		procedure unit_not_deployed is 
			use pac_unit_name;
		begin
			set_status ("ERROR: Unit " & to_string (unit_name) 
				& " of device " & to_string (device_name) 
				& " not deployed !");
		end unit_not_deployed;

		
		procedure unit_in_use is 
			use pac_unit_name;
		begin
			set_status ("ERROR: Unit " & to_string (unit_name) 
				& " of device " & to_string (device_name) 
				& " already in use !");
			-- CS output coordinates of used unit
		end unit_in_use;

		
		procedure unit_not_on_this_sheet is 
			use pac_unit_name;
		begin
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

					device_name := to_device_name (get_field (5));

					if device_exists (active_module, device_name) then

						unit_add.device		:= device_model_cursor (active_module, device_name);
						
						--unit_add.variant	:= device_variant_name (active_module, device_name);
						-- CS: really required ? requires test whether the device is real
						
						unit_add.total		:= get_unit_count (unit_add.device);
						unit_add.device_pre	:= device_name;
					
						menu_propose_units_on_fetch (
							device			=> device_name,
							units			=> get_available_units (
												active_module,
												device_name,
												log_threshold + 1),
							log_threshold	=> log_threshold + 1);

					else
						device_not_found;
					end if;

					
				when 6 => -- like "fetch unit IC1 B"
					device_name := to_device_name (get_field (5));

					if device_exists (active_module, device_name) then

						unit_add.device		:= device_model_cursor (active_module, device_name);

						--unit_add.variant	:= device_variant_name (active_module, device_name);
						-- CS: really required ? requires test whether the device is real

						unit_add.total		:= get_unit_count (unit_add.device);
						unit_add.device_pre	:= device_name;
						
						unit_name := to_unit_name (get_field (6));

						-- test existence AND availability of unit:
						if provides_unit (unit_add.device, unit_name) then

							if unit_available (active_module, device_name, unit_name) then

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
		
		-- Missing arguments are to be proposed only if
		-- the command origin is the console.
		-- Otherwise nothing happens here.
		if not is_complete (cmd) and get_origin (cmd) = ORIGIN_CONSOLE then
			
			log_command_incomplete (cmd_field_count, log_threshold);

			-- There might be objects such as net segments or units selected.
			-- They must be de-selected:
			clear_proposed_objects; -- CS remove
			
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then

										-- unit_delete.device := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_delete (
											-- device			=> unit_delete.device,
											device			=> device_name,
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;

									
								when 6 => -- like "delete unit IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then
										
										--unit_delete.device := device_name;

										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed on the 
										-- current active sheet.
										-- Deleting is possible if it is deployed 
										-- and if it is on the current sheet.
										if is_deployed (active_module, device_name, unit_name) then

											-- unit_delete.unit := unit_name;
											
											if get_sheet (active_module, device_name, unit_name) = active_sheet then

												delete_unit (
													module_cursor	=> active_module,
													device_name		=> device_name,
													unit_name		=> unit_name,
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then
										object_device_name := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_move (
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;
									
								when 6 => -- like "drag unit IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then
										
										object_device_name := device_name;

										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed on the current active sheet.
										-- Dragging is possible if it is deployed and if it is on the current sheet.
										-- It will then be attached to the cursor or mouse pointer.
										if is_deployed (active_module, object_device_name, unit_name) then

											object_unit_name := unit_name;
											
											if get_sheet (active_module, object_device_name, object_unit_name) = active_sheet then
												select_unit_for_move;
												
												-- use the current primary tool for moving the unit:
												object_tool := primary_tool;

												find_attached_segments;
												
												set_edit_process_running;

												set_finalization_pending (cmd);
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
							null;
							
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then
										object_device_name := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_move (
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;
									
								when 6 => -- like "move unit IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then
										
										object_device_name := device_name;

										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed.
										-- If it is deployed somewhere (whatever sheet) then it will be 
										-- attached to the cursor or mouse pointer.
										if is_deployed (active_module, object_device_name, unit_name) then

											object_unit_name := unit_name;
											
											-- If the unit is not on the active_sheet then notify the
											-- GUI that the sheet changes. This way the unit is drawn
											-- on the current visible sheet independed of its original sheet number.
											-- See et_canvas_schematic.draw_units.
											if get_sheet (active_module, object_device_name, object_unit_name) /= active_sheet then
												object_sheet_changes := true;

												--set_status ("Moving unit from another sheet");
											end if;

											select_unit_for_move;
											
											-- use the current primary tool for moving the unit:
											object_tool := primary_tool;
										
											set_edit_process_running;

											set_finalization_pending (cmd);
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then

										placeholder_move.device := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_move (
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;

								when 6 => -- like "move name IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then

										placeholder_move.device := device_name;
										
										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed on the current active sheet.
										-- Moving the placeholder is possible if the unit it is deployed 
										-- and if it is on the current sheet.
										-- The placeholder will then be attached to the cursor or mouse pointer.
										if is_deployed (active_module, placeholder_move.device, unit_name) then

											placeholder_move.unit := unit_name;
											
											if get_sheet (active_module, placeholder_move.device, placeholder_move.unit) = active_sheet then
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then
										object_device_name := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_move (
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;
									
								when 6 => -- like "rotate unit IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then
										
										object_device_name := device_name;

										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed on the current active sheet.
										-- Rotating is possible if it is deployed and if it is on the current sheet.
										-- It will then be attached to the cursor or mouse pointer.
										if is_deployed (active_module, object_device_name, unit_name) then

											object_unit_name := unit_name;
											
											if get_sheet (active_module, object_device_name, object_unit_name) = active_sheet then
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

									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then

										placeholder_move.device := device_name;

										-- Propose units that are on the current active sheet:
										menu_propose_units_on_move (
											units			=> get_units_on_sheet (
																active_module,
																device_name,
																active_sheet,
																log_threshold + 1),
											log_threshold	=> log_threshold + 1);

									else
										device_not_found;
									end if;

								when 6 => -- like "rotate name IC1 B"
									device_name := to_device_name (get_field (5));
									
									if device_exists (active_module, device_name) then

										placeholder_move.device := device_name;
										
										unit_name := to_unit_name (get_field (6));

										-- Test whether the unit is deployed on the current active sheet.
										-- Rotating the placeholder is possible if the unit it is deployed 
										-- and if it is on the current sheet.
										-- The placeholder will then be attached to the cursor or mouse pointer.
										if is_deployed (active_module, placeholder_move.device, unit_name) then

											placeholder_move.unit := unit_name;
											
											if get_sheet (active_module, placeholder_move.device, placeholder_move.unit) = active_sheet then
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
									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then
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
									device_name := to_device_name (get_field (5));

									if device_exists (active_module, device_name) then
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
		end if;
	end propose_arguments;



	
	
begin
	log (text => "execute schematic command: " & enclose_in_quotes (get_all_fields (cmd)),
		 level => log_threshold);

	log (text => "command origin: " & get_origin (cmd), level => log_threshold);


	
	module := to_module_name (get_field (2)); -- motor_driver (without extension *.mod)
	-- CS: Becomes obsolete once all board ops use the
	-- given module_cursor.
	

	set_verb_and_noun;
	

	-- Once verb and noun are known, they must be shown
	-- in the verb-noun-display:
	update_verb_noun_display;
	
	
	-- Parse the command:	
	parse;
	

	-- If the command is incomplete and if it was entered
	-- via the console, then further arguments are proposed.
	-- Otherwise nothing happens here:	
	propose_arguments;


	evaluate_command_exit_code (cmd, log_threshold);

	
	-- After each command (regardless if it is complete or not)
	-- set the focus to the canvas:
	-- CS: remove ?
	-- if runmode /= MODE_HEADLESS then
	-- 	canvas.grab_focus; -- NOTE ! calls "cb_draw"
	-- end if;
	
	-- exception when event: others =>

			-- CS
			-- evaluate_exception (
			-- 	name	=> exception_name (event),
			-- 	message	=> exception_message (event));

			-- raise;
			
end execute_schematic_command;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
