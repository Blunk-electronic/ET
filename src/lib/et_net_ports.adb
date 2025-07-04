------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NET PORTS                                   --
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

with et_module_names;


package body et_net_ports is


	
	function "<" (left, right : in type_device_port) return boolean is
		use pac_port_name;
		use pac_unit_name;
	begin
		-- compare device names:
		if left.device_name < right.device_name then
			return true;
			
		elsif left.device_name = right.device_name then

			
			-- compare unit names:
			if left.unit_name < right.unit_name then
				return true;
				
			elsif left.unit_name = right.unit_name then


				-- compare port names:
				if left.port_name < right.port_name then
					return true;
				else
					return false;
				end if;

			else
				return false;
			end if;

			
		else
			return false;
		end if;
	end;


	
	
	function "<" (left, right : in type_submodule_port) return boolean is
		use et_module_names;
		use pac_module_instance_name;
		use et_net_names.pac_net_name;
	begin
		if left.module_name < right.module_name then
			return true;
		elsif left.module_name > right.module_name then
			return false;
		elsif left.port_name < right.port_name then
			return true;
		else
			return false;
		end if;
	end;




	
	
	function to_string (
		port : in type_device_port) 
		return string 
	is 
		use pac_unit_name;
	begin
		return "device " & to_string (port.device_name)
			& " unit " & to_string (port.unit_name)
			& " port " & to_string (port.port_name);
	end to_string;



	
	
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean)
	is
		use pac_device_ports;
		c : pac_device_ports.cursor := ports.first;
	begin
		while c /= pac_device_ports.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;




	function merge_ports (
		right, left : in type_ports)
		return type_ports
	is
		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
		
		result : type_ports := left;

	begin
		union (result.devices, right.devices);
		union (result.submodules, right.submodules);
		union (result.netchangers, right.netchangers);
		
		return result;
	end merge_ports;


	
	

	function in_ports (
		ports	: in type_ports;
		port	: in et_netlists.type_port_netchanger)
		return boolean
	is
		result : boolean := false;

		use et_netlists;
		use pac_netchanger_ports;
		port_cursor : pac_netchanger_ports.cursor;
	begin
		port_cursor := ports.netchangers.find (port);
		
		if has_element (port_cursor) then
			result := true;
		end if;

		return result;
	end in_ports;





	function in_ports (
		ports	: in type_ports;
		port	: in type_submodule_port)
		return boolean
	is
		result : boolean := false;

		use pac_submodule_ports;
		port_cursor : pac_submodule_ports.cursor;
	begin
		port_cursor := ports.submodules.find (port);
		
		if has_element (port_cursor) then
			result := true;
		end if;

		return result;
	end in_ports;



	

	function no_ports (
		ports : in type_ports) 
		return boolean 
	is
		result : boolean := true;
		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		if length (ports.devices) > 0 then
			return false;
		end if;

		if length (ports.submodules) > 0 then
			result := false;
		end if;

		if length (ports.netchangers) > 0 then
			result := false;
		end if;

		return result;
	end no_ports;

	
	
end et_net_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
