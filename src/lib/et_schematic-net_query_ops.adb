------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    SCHEMATIC NET QUERY OPERATIONS                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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


package body et_schematic.net_query_ops is


	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor)
		return type_ports
	is
		ports : type_ports; -- to be returned

		use pac_nets;
		use pac_strands;
		use pac_net_segments;

		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			
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
			-- Collect device ports of the segment according to given assembly variant:
			iterate (element (segment_cursor).ports_devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			--union (result.netchangers, element (segment_cursor).ports_netchangers);
			--union (result.submodules, element (segment_cursor).ports_submodules);
			null;
		end query_segments;


		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		iterate (element (net).strands, query_strands'access);
		
		return ports;
	end get_ports;

	
		
end et_schematic.net_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
