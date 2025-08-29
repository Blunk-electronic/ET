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
with ada.characters.handling;

with et_keywords;				use et_keywords;
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



	function to_device_port (
		device	: in type_device_name;
		unit	: in pac_unit_name.bounded_string;
		port	: in pac_port_name.bounded_string)
		return type_device_port
	is 
		result : type_device_port;
	begin
		result.device_name := device;
		result.unit_name := unit;
		result.port_name := port;
		return result;
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



	

	procedure make_device_port (
		arguments	: in type_fields_of_line; -- device IC1 unit C port I1
		error		: out boolean;
		port		: out type_device_port)
	is
		use ada.characters.handling;
		
		function f (place : in type_field_count_positive) 
			return string 
		is begin
			return to_lower (get_field (arguments, place));
		end;
		
	begin
		error := false;
		
		-- Iterate all fields of given list of arguments:
		-- P points to the place in arguments at which we 
		-- fetch a field from.
		-- If something goes wrong, then the error-flag is
		-- set and the iteration cancelled:
		for p in 1 .. get_field_count (arguments) loop
			
			case p is
				when 1 => -- device
					if f (p) /= keyword_device then
						error := true;
						exit;
					end if;

				when 2 => -- IC1
					port.device_name := to_device_name (f (p));
					-- CS check existence of device in schematic

				when 3 => -- unit
					if f (p) /= keyword_unit then
						error := true;
						exit;
					end if;

				when 4 => -- C
					port.unit_name := to_unit_name (f (p));
					-- CS check existenc of unit in schematic and model

				when 5 => -- port
					if f (p) /= keyword_port then
						error := true;
						exit;
					end if;

				when 6 => -- I1
					port.port_name := to_port_name (f (p));
					-- CS check existence of port in model

				when others =>					
					error := true;
					exit;
			end case;
		end loop;
	end make_device_port;


	

	
	
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


	

	function to_string (
		port : in pac_device_ports.cursor) 
		return string 
	is 
		p : type_device_port renames element (port);
	begin
		return to_string (p);
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



	
	procedure merge_ports (
		target	: in out type_ports;
		source	: in type_ports)
	is
		scratch : type_ports;
	begin
		scratch := merge_ports (target, source);
		target := scratch;
	end;

	
	

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

	


	function get_port_count (
		ports : in type_ports)
		return natural
	is 
		result : natural := 0;

		d, s, n : count_type := 0;

		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		d := length (ports.devices);
		s := length (ports.submodules);
		n := length (ports.netchangers);

		result := natural (d + s + n);
		return result;
	end;

		

	
	
end et_net_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
