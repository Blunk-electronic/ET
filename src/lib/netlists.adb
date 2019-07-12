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

	function "<" (left, right : in type_port) return boolean is
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

-- 	function extend_ports (
-- 	-- Adds further properties to the given ports (characteristics, terminal name).
-- 		ports : in et_schematic.type_ports_device.set)
-- 		return netlists.type_ports.set is
-- 
-- 		ports_ext : type_ports.set; -- to be returned
-- 
-- 		use et_schematic.type_ports_device;
-- 		
-- 		procedure query_ports (port_cursor : in et_schematic.type_ports_device.cursor) is
-- 			port_sch : et_schematic.type_port_device := element (port_cursor);
-- 		begin
-- 
-- 			insert (
-- 				container	=> ports_ext,
-- 				new_item	=> (
-- 					direction	=> et_libraries.PASSIVE, -- CS
-- 					device		=> port_sch.device_name, -- IC1
-- 					port		=> port_sch.port_name, -- CE
-- 					others		=> <>) -- CS
-- 				   );
-- 			
-- 		end query_ports;
-- 		
-- 	begin -- extend_ports
-- 		iterate (ports, query_ports'access);
-- 		return ports_ext;
-- 	end extend_ports;
	
	procedure write_netlist (
	-- Creates the netlist (which inevitably and intentionally overwrites the previous file).
	-- Writes the content of the given container netlist in the file.
		netlist			: in type_netlist.map;
		module_name		: in type_module_name.bounded_string; -- motor_driver
		file_name		: in type_file_name.bounded_string; -- netlist.net
		log_threshold	: in type_log_level) is		

		use type_netlist;
		netlist_handle : ada.text_io.file_type;

		procedure write_header is begin
		-- writes a nice header in the netlist file
			put_line (netlist_handle, comment_mark & " " & et_general.system_name & " " & et_general.version & " netlist");
			put_line (netlist_handle, comment_mark & " " & date);
			put_line (netlist_handle, comment_mark & " module " & enclose_in_quotes (to_string (module_name)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (length (netlist)));
			-- CS: statistics about pin count ?
			
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net_name");
			put_line (netlist_handle, comment_mark & "  device port direction terminal/pin/pad");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);
		end write_header;
		
		procedure write_nets is
		-- writes the actual nets in the netlist file

			procedure query_ports (port : in type_ports.cursor) is
				use type_ports;
			begin
				put_line (netlist_handle, -- IC1 CE input H5
					et_libraries.to_string (element (port).device) & latin_1.space &
					et_libraries.to_string (element (port).port) & latin_1.space &
					et_libraries.to_string (element (port).direction) & latin_1.space &
					et_libraries.to_string (element (port).terminal) & latin_1.space);

					-- CS .characteristics
			end query_ports;
			
			procedure query_nets (net : in type_netlist.cursor) is begin
				new_line (netlist_handle);

				-- write the net name
				put_line (netlist_handle, to_string (key (net))); -- clock_out

				-- write the device ports
				iterate (element (net), query_ports'access);

			end query_nets;

			
		begin -- write_nets
			iterate (netlist, query_nets'access);
		end write_nets;

		procedure write_footer is begin
		-- writes a nice footer in the netlist file
			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
		end write_footer;
		
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

		write_header;
		write_nets;
		write_footer;
		
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

	
end netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
