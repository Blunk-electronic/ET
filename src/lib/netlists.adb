------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NETLISTS                                    --
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
with ada.exceptions;

-- with et_general;				use et_general;
-- 
-- with et_coordinates;
-- with et_libraries;
-- with assembly_variants;
with et_string_processing;		use et_string_processing;
with et_csv;					use et_csv;
-- with et_pcb_coordinates;
-- with submodules;
-- with numbering;

package body netlists is
	
	function to_string (name : in type_file_name.bounded_string) return string is begin
		return type_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return type_file_name.bounded_string is begin
		return type_file_name.to_bounded_string (name);
	end;

	function "<" (left, right : in type_node) return boolean is
		use et_libraries;
		use type_port_name;
		result : boolean := false;
	begin
		if left.device < right.device then
			result := true;
			
		elsif left.device = right.device then

			if left.port < right.port then
				result := true;
			else
				result := false;
			end if;
			
		else
			result := false;
		end if;
		
		return result;
	end;
	
	procedure write_netlist (
	-- Creates the netlist (which inevitably and intentionally overwrites the previous file).
	-- Writes the content of the given container netlist in the file.
		netlist			: in type_netlist.map;
		file_name		: in type_file_name.bounded_string;
		log_threshold	: in type_log_level) is		

		netlist_handle : ada.text_io.file_type;
		net_cursor : type_netlist.cursor := netlist.first;

-- 		procedure native is -- CS not complete. accessories ?
-- 			use et_csv;
-- 
-- 			column_item			: constant string := "ITEM";
-- 			column_device		: constant string := "DEVICE";			
-- 			column_value		: constant string := "VALUE";
-- 			column_package		: constant string := "PACKAGE";
-- 			column_partcode		: constant string := "PARTCODE";
-- 			column_purpose		: constant string := "PURPOSE";
-- 
-- 			procedure query_device (cursor : in type_devices.cursor) is
-- 				use type_devices;
-- 				use et_libraries;
-- 			begin
-- 				put_field (file => bom_handle); -- CS item number
-- 				put_field (file => bom_handle, text => to_string (key (cursor))); -- R4
-- 				put_field (file => bom_handle, text => to_string (element (cursor).value)); -- 100R
-- 				put_field (file => bom_handle, text => to_string (element (cursor).packge)); -- S_0805.pac
-- 				put_field (file => bom_handle, text => to_string (element (cursor).partcode)); -- R_PAC_S_0805_VAL_100R
-- 				put_field (file => bom_handle, text => to_string (element (cursor).purpose)); -- purpose
-- 
-- 				put_lf (file => bom_handle, field_count => et_csv.column);
-- 			end query_device;
-- 			
-- 		begin -- native
-- 			-- CS: A nice header should be placed. First make sure stock_manager can handle it.
-- 			
-- 			-- write the BOM table header
-- 			et_csv.reset_column;
-- 			put_field (file => bom_handle, text => column_item);
-- 			put_field (file => bom_handle, text => column_device);
-- 			put_field (file => bom_handle, text => column_value);
-- 			put_field (file => bom_handle, text => column_package);
-- 			put_field (file => bom_handle, text => column_partcode);
-- 			put_field (file => bom_handle, text => column_purpose);
-- 			put_lf    (file => bom_handle, field_count => et_csv.column);
-- 
-- 			type_devices.iterate (
-- 				container	=> bom,
-- 				process		=> query_device'access);
-- 			
-- 			-- CS: A list end mark should be placed. First make sure stock_manager can handle it.
-- 			-- put_line (bom_handle, comment_mark & " end of list");
-- 			
-- 		end;
-- 
	begin -- write_netlist
		if ada.directories.exists (to_string (file_name)) then
			log (importance => NOTE, text => "overwriting " & to_string (file_name) & " ...", level => log_threshold);
		end if;

		if ada.directories.extension (to_string (file_name)) /= extension_netlist then
			log (importance => WARNING, text => "targeted netlist file has no extension " &
				 enclose_in_quotes (extension_netlist) & " !");
		end if;

		log (text => "writing netlist file " & enclose_in_quotes (to_string (file_name)) & " ...", level => log_threshold);
		
		create (
			file => netlist_handle,
			mode => out_file, 
			name => to_string (file_name));

		close (netlist_handle);

		exception
			when event: others =>
				if is_open (netlist_handle) then
					close (netlist_handle);
				end if;
				
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
				
	end write_netlist;


-- 	procedure export_netlists (log_threshold : in et_string_processing.type_log_level) is
-- 	-- Exports/Writes the netlists in separate files.
-- 	-- Netlists are exported in individual project directories in the work directory of ET.
-- 	-- These project directories have the same name as the module indicated by module_cursor.
-- 	-- Addresses real components exclusively. Virtual things like GND symbols are not exported.
-- 	-- Call this procedure after executing procedure make_netlist !
-- 		use type_modules;
-- 		use ada.directories;
-- 		use et_general;
-- 		use et_string_processing;
-- 		use et_export;
-- 		use et_coordinates;
-- 		
-- 		netlist_handle 		: ada.text_io.file_type;
-- 		netlist_file_name	: et_schematic.type_netlist_file_name.bounded_string;
-- 		
-- 		procedure query_nets (
-- 			module_name	: in type_submodule_name.bounded_string;
-- 			module		: in type_module) is
-- 			net_cursor	: type_netlist.cursor := module.netlist.first;
-- 
-- 			procedure query_ports (
-- 				net_name	: in type_net_name.bounded_string;
-- 				ports		: in type_ports_with_reference.set) is
-- 				port_cursor : type_ports_with_reference.cursor := ports.first;
-- 		
-- 				terminal : et_libraries.type_terminal;
-- 
-- 			begin
-- 				log_indentation_up;
-- 				--log (text => "ports" & count_type'image (length (ports)), level => log_threshold + 3);
-- 
-- 				while type_ports_with_reference."/=" (port_cursor, type_ports_with_reference.no_element) loop
-- 
-- 					-- we export only ports of real components
-- 					if et_libraries."=" (type_ports_with_reference.element (port_cursor).appearance, et_libraries.sch_pcb) then
-- 
-- 						-- fetch the terminal from the port in the current module
-- 						terminal := to_terminal (
-- 							port 			=> type_ports_with_reference.element (port_cursor),
-- 							module 			=> key (module_cursor),	-- nucleo_core
-- 							log_threshold 	=> log_threshold + 4);
-- 					
-- 						-- log reference, unit, port, direction, terminal (all in one line)
-- 						log (text => "reference " & et_libraries.to_string (type_ports_with_reference.element (port_cursor).reference)
-- 							& " unit " & et_libraries.to_string (unit_name => terminal.unit)
-- 							& " port " & et_libraries.to_string (port => terminal.port)
-- 							& to_string (type_ports_with_reference.element (port_cursor).direction)
-- 							& " terminal " & et_libraries.to_string (terminal => terminal.name),
-- 							level => log_threshold + 3);
-- 
-- 						-- write reference, port, direction, terminal in netlist (all in one line)
-- 						put_line (netlist_handle, 
-- 							et_libraries.to_string (type_ports_with_reference.element (port_cursor).reference) & latin_1.space
-- 							& et_libraries.to_string (port => terminal.port)
-- 							& to_string (type_ports_with_reference.element (port_cursor).direction, preamble => false) & latin_1.space
-- 							& et_libraries.to_string (terminal => terminal.name)); 
-- 
-- 					end if;
-- 						
-- 					type_ports_with_reference.next (port_cursor);
-- 				end loop;
-- 
-- 				log_indentation_down;
-- 			end query_ports;
-- 			
-- 		begin -- query_nets
-- 			log_indentation_up;
-- 
-- 			-- output the net names. then query the ports/pins of the net
-- 			while type_netlist."/=" (net_cursor, type_netlist.no_element) loop
-- 
-- 				-- log and write net name in netlist
-- 				log (text => et_general.to_string (type_netlist.key (net_cursor)), level => log_threshold + 2);
-- 				new_line (netlist_handle);
-- 				put_line (netlist_handle, et_general.to_string (type_netlist.key (net_cursor)));
-- 
-- 				-- query ports of net
-- 				type_netlist.query_element (
-- 					position	=> net_cursor,
-- 					process		=> query_ports'access);
-- 				
-- 				type_netlist.next (net_cursor);
-- 			end loop;
-- 				
-- 			log_indentation_down;	
-- 		end query_nets;
-- 
-- 	begin -- export_netlists
-- 
-- 		-- We start with the first module of the modules.
-- 		--first_module;
-- 		module_cursor := type_modules.first (modules);
-- 
-- 		log (text => "exporting netlists ...", level => log_threshold);
-- 		log_indentation_up;
-- 		
-- 		while module_cursor /= type_modules.no_element loop
-- 			log (text => "module " & to_string (key (module_cursor)), level => log_threshold);
-- 			log_indentation_up;
-- 
-- 			create_project_directory (to_string (key (module_cursor)), log_threshold + 2);			
-- 			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
-- 			netlist_file_name := et_schematic.type_netlist_file_name.to_bounded_string 
-- 				(
-- 				compose (
-- 					containing_directory => compose 
-- 						(
-- 						containing_directory => compose (work_directory, to_string (key (module_cursor))),
-- 						name => et_export.directory_cam
-- 						),
-- 					name => to_string (key (module_cursor)),
-- 					extension => et_schematic.extension_netlist)
-- 				);
-- 
-- 			-- create the netlist (which inevitably and intentionally overwrites the previous file)
-- 			log (text => "creating netlist file " & et_schematic.type_netlist_file_name.to_string (netlist_file_name), level => log_threshold + 1);
-- 			create (
-- 				file => netlist_handle,
-- 				mode => out_file, 
-- 				name => et_schematic.type_netlist_file_name.to_string (netlist_file_name));
-- 
-- 			put_line (netlist_handle, comment_mark & " " & et_general.system_name & " " & et_general.version & " netlist");
-- 			put_line (netlist_handle, comment_mark & " " & date);
-- 			put_line (netlist_handle, comment_mark & " module " & to_string (key (module_cursor)));
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_double);
-- 			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (net_count));
-- 			-- CS: statistics about pin count ?
-- 			
-- 			put_line (netlist_handle, comment_mark);
-- 			put_line (netlist_handle, comment_mark & " legend:");
-- 			put_line (netlist_handle, comment_mark & "  net_name");
-- 			put_line (netlist_handle, comment_mark & "  reference/component port direction terminal/pin/pad");
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_single);
-- 
-- 			-- do the export
-- 			query_element (
-- 				position	=> module_cursor,
-- 				process		=> query_nets'access);
-- 
-- 			new_line (netlist_handle);
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_double);
-- 			put_line (netlist_handle, comment_mark & " end of list");
-- 			
-- 			close (netlist_handle);
-- 			log_indentation_down;
-- 			
-- 			next (module_cursor);
-- 			
-- 		end loop;
-- 			
-- 		log_indentation_down;
-- 		
-- 	end export_netlists;

	
end netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
