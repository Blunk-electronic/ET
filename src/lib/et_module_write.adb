------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             MODULE WRITE                                 --
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
-- - simplify code
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;

with ada.directories;				use ada.directories;
with ada.exceptions;
with gnat.directory_operations;
with et_directory_and_file_ops;

with et_file_write;					use et_file_write;
with et_system_info;
with et_time;						use et_time;
with et_string_processing;			use et_string_processing;

with et_section_headers;			use et_section_headers;

with et_pcb_sides;
with et_board_layer_category;			use et_board_layer_category;

with et_module_write_meta;				use et_module_write_meta;
with et_module_write_grid;				use et_module_write_grid;
with et_module_write_frames;			use et_module_write_frames;
with et_module_write_design_rules;		use et_module_write_design_rules;
with et_module_write_pcb_layer_stack;	use et_module_write_pcb_layer_stack;
with et_module_write_board_outline;		use et_module_write_board_outline;
with et_module_write_freetracks;		use et_module_write_freetracks;
with et_module_write_board_zones;		use et_module_write_board_zones;
with et_module_write_text_board;		use et_module_write_text_board;
with et_module_write_text_schematic;	use et_module_write_text_schematic;
with et_module_write_silkscreen;		use et_module_write_silkscreen;
with et_module_write_assy_doc;			use et_module_write_assy_doc;
with et_module_write_stopmask;			use et_module_write_stopmask;
with et_module_write_stencil;			use et_module_write_stencil;
with et_module_write_route_restrict;	use et_module_write_route_restrict;
with et_module_write_nets;				use et_module_write_nets;
with et_module_write_net_classes;		use et_module_write_net_classes;
with et_module_write_netchangers;		use et_module_write_netchangers;
with et_module_write_submodules;		use et_module_write_submodules;

with et_module_write_board_user_settings;	use et_module_write_board_user_settings;
with et_module_write_devices_electrical;	use et_module_write_devices_electrical;
with et_module_write_device_non_electrical;	use et_module_write_device_non_electrical;
with et_module_write_assembly_variants;		use et_module_write_assembly_variants;


package body et_module_write is

	

	procedure write_module (
		module_cursor	: in pac_generic_modules.cursor;
		save_as_name	: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver_test, templates/clock_generator_test
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;

		-- backup the previous output destination
		previous_output : ada.text_io.file_type renames current_output;
		
		module_file_handle : ada.text_io.file_type;


		-- Composes the target file name like bood-sample-tester.mod,
		-- creates the file (in the current working directory) and 
		-- directs all outputs to it.
		-- Writes s a nice header in the target file:
		procedure write_header is 
			use ada.directories;
			use gnat.directory_operations;
			-- use pac_project_name;
			-- use pac_project_path;
			use et_system_info;
		begin
			if pac_module_name.length (save_as_name) = 0 then
				-- The module is to be saved with its own name:

				log (text => "Saving module as " 
					& enclose_in_quotes (to_string (key (module_cursor))) & " ...",
					level => log_threshold + 1);
					
				-- Compose the target full file name and create the module file:
				create (
					file => module_file_handle,
					mode => out_file, 
					name => to_string (key (module_cursor))
							& latin_1.full_stop
							& module_file_name_extension
					);
			else
				-- The module is to be saved with a different name:

				log (text => "Saving module as " 
					& enclose_in_quotes (to_string (save_as_name)) & " ...",
					level => log_threshold + 1);

				create (
					file => module_file_handle,
					mode => out_file, 
					name => to_string (save_as_name)
							& latin_1.full_stop
							& module_file_name_extension
					);

			end if;
			
			-- write in a nice header
			set_output (module_file_handle);
			put_line (comment_mark_default & " " & system_name & " module");
			put_line (comment_mark_default & " " & get_date);
			put_line (comment_mark_default & " " & row_separator_double);
			new_line;
		end write_header;		


		

		-- Writes a nice footer in the target file and closes it.
		-- Directs subsequent outputs to the previous output (That
		-- is the output which was set before write_module has been called.):
		procedure write_footer is begin
			new_line;		
			log (text => "closing module file ...", level => log_threshold + 1);
			
			put_line (comment_mark_default & " " & row_separator_double);
			put_line (comment_mark_default & " " & get_date);
			put_line (comment_mark_default & " module file end");
			new_line;

			set_output (previous_output);
			close (module_file_handle);
		end write_footer;




		
		procedure query_board is

			procedure write_silkscreen is
				use et_pcb_sides;
			begin
				section_mark (section_silkscreen, HEADER);

				section_mark (section_top, HEADER);
				
				-- lines, arcs, circles:
				write_silkscreen (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_silkscreen (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);

				section_mark (section_bottom, FOOTER);
				
				section_mark (section_silkscreen, FOOTER);
			end write_silkscreen;


			
			procedure write_assy_doc is
				use et_pcb_sides;
			begin
				section_mark (section_assembly_doc, HEADER);

				section_mark (section_top, HEADER);

				-- lines, arcs, circles:
				write_assy_doc (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_assy_doc (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_assembly_doc, FOOTER);
			end write_assy_doc;
			

			
			procedure write_stencil is
				use et_pcb_sides;
			begin			
				section_mark (section_stencil, HEADER);

				section_mark (section_top, HEADER);
				write_stencil (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STENCIL, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				write_stencil (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STENCIL, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_stencil, FOOTER);
			end write_stencil;


			
			procedure write_stop_mask is
				use et_pcb_sides;
			begin
				section_mark (section_stopmask, HEADER);

				section_mark (section_top, HEADER);

				-- lines, arcs, circles:
				write_stopmask (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_stopmask (module_cursor, BOTTOM, log_threshold + 2);				
				write_zones_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_stopmask, FOOTER);
			end write_stop_mask;



			procedure write_keepout is
				use et_pcb_sides;
			begin
				section_mark (section_keepout, HEADER);

				section_mark (section_top, HEADER);
				write_zones_non_conductor (module_cursor, LAYER_CAT_KEEPOUT, TOP, log_threshold + 2);
				write_zones_non_conductor_cutout (module_cursor, LAYER_CAT_KEEPOUT, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				write_zones_non_conductor (module_cursor, LAYER_CAT_KEEPOUT, BOTTOM, log_threshold + 2);
				write_zones_non_conductor_cutout (module_cursor, LAYER_CAT_KEEPOUT, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_keepout, FOOTER);
			end write_keepout;

			

			procedure write_route_restrict is begin
				section_mark (section_route_restrict, HEADER);

				-- lines, arcs, circles:
				write_route_restrict (module_cursor, log_threshold + 2);
				write_zones_route_restrict (module_cursor, log_threshold + 2);
				write_zones_route_restrict_cutout (module_cursor, log_threshold + 2);

				section_mark (section_route_restrict, FOOTER);
			end write_route_restrict;



			procedure write_via_restrict is begin
				section_mark (section_via_restrict, HEADER);
				write_zones_via_restrict (module_cursor, log_threshold + 2);
				write_zones_via_restrict_cutout (module_cursor, log_threshold + 2);
				section_mark (section_via_restrict, FOOTER);
			end write_via_restrict;



			procedure write_conductors_floating is begin
				section_mark (section_conductor, HEADER);

				write_freetracks (module_cursor, log_threshold + 2);
				write_zones_conductor (module_cursor, log_threshold + 2);
				write_zones_conductor_cutout (module_cursor, log_threshold + 2);
				write_texts_conductor (module_cursor, log_threshold + 2);
				write_placeholders_conductor (module_cursor, log_threshold + 2);

				section_mark (section_conductor, FOOTER);
			end;

			
		begin -- query_board
			section_mark (section_board, HEADER);

			write_board_user_settings (module_cursor, log_threshold + 1);
		
			write_devices_non_electrical (module_cursor, log_threshold + 1);

			write_silkscreen;
			write_assy_doc;
			write_stencil;
			write_stop_mask;
			write_keepout;
			
			write_route_restrict;
			write_via_restrict;

			write_conductors_floating; -- NON-ELECTRIC !

			-- outer board contour and holes:
			write_board_outline (module_cursor, log_threshold + 1);
			
			---BOARD END-----
			section_mark (section_board, FOOTER);
		end query_board;


		
	begin -- write_module

		-- CS check if module is inside project directory ?
		
		write_header;

		-- meta data
		write_meta (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- rules
		write_design_rules (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- net classes
		write_net_classes (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- drawing grid
		write_drawing_grid (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- layer stack
		write_layer_stack (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- nets
		write_nets (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- frames
		write_frames (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- scheamtic texts / notes
		write_schematic_texts (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- submodules
		write_submodules (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- devices
		write_devices_electrical (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- assembly variants
		write_assembly_variants (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- netchangers
		write_netchangers (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- board
		query_board;
		put_line (row_separator_single);	

		write_footer;

		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				close (module_file_handle);
				set_output (previous_output);
				raise;
		
	end write_module;


end et_module_write;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
