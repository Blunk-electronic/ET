------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
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
-- <http://www.gnu.org/licenses/>.   
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;

with ada.characters.handling;	use ada.characters.handling;
with ada.exceptions;

with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_device_model;


package body et_schematic is

	
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports 
	is
		result : type_ports; -- to be returned

		use pac_nets;
		use pac_strands;
		use pac_net_segments;

		
		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			use pac_device_ports;

			use et_netlists;
			use pac_netchanger_ports;			
			use pac_submodule_ports;
			
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			procedure query_devices (device_cursor : in pac_device_ports.cursor) is begin
				if et_assembly_variants.is_mounted (
					device		=> element (device_cursor).device_name, -- IC4, R101
					variant		=> variant) 
				then
					--put_line (to_string (element (device_cursor)));
					
					insert (
						container	=> result.devices,
						new_item	=> element (device_cursor));
				end if;

				exception
					when event: others =>
						raise constraint_error with to_string (element (device_cursor))
						--put_line (to_string (element (device_cursor))
						& " already in set !";
						
			end query_devices;

			
		begin
			-- Collect device ports of segment according to given assembly variant.
			iterate (element (segment_cursor).ports.devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports.netchangers);
			union (result.submodules, element (segment_cursor).ports.submodules);
		end query_segments;

		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		--put_line ("net " & to_string (key (net)));		
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;


	
	

	
	procedure device_name_in_use (
		name	: in type_device_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category)	-- electrical/non-electrical
	is begin
		case by_cat is
			when NON_ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another non-electrical device !",
					 console => true);

			when ELECTRICAL =>
				log (ERROR, "Name " & enclose_in_quotes (to_string (name)) &
					 " already used by another electrical device !",
					 console => true);
		end case;
		
		raise constraint_error;
	end device_name_in_use;

		
end et_schematic;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
