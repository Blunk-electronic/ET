------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   SCHEMATIC DEVICE QUERY OPERATIONS                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

package body et_schematic.device_query_ops is

	function get_port (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminals.cursor)
		return pac_port_name.bounded_string
	is
		result : pac_port_name.bounded_string;

		-- Get the full device model:
		device_model : pac_devices_lib.cursor := locate_device (pac_devices_sch.element (device).model);

		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib)
		is
		begin
			--query_element (model.
			null;
		end query_model;
		
		use pac_devices_lib;
	begin
		-- The given device must be real. Otherwise we return an empty string.
		if pac_devices_sch.element (device).appearance = PCB then
			query_element (device_model, query_model'access);
		else
			result := to_port_name ("");
		end if;
			
		return result;
	end get_port;

	
	
	function get_net (
		device		: in pac_devices_sch.cursor;
		terminal	: in et_terminals.pac_terminals.cursor)
		return pac_nets.cursor
	is
		result : pac_nets.cursor;

		use pac_devices_sch;
		--use et_devices;
		device_model : pac_devices_lib.cursor := locate_device (element (device).model);

		procedure query_model (
			model	: in pac_device_model_file.bounded_string;
			device	: in type_device_lib)
		is
		begin
			null;
		end query_model;
		
		use pac_devices_lib;
	begin 
		query_element (device_model, query_model'access);
		--device_model 
		-- element (device).model
		-- element (device).variant
		return result;
	end get_net;


	
			
end et_schematic.device_query_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
