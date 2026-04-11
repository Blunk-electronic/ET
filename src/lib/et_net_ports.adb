------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           NET SEGMENT PORTS                              --
--                                                                          --
--                               B o d y                                    --
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
-- <http://www.gnu.org/licenses/>.   
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

with ada.text_io;				use ada.text_io;
with ada.characters.handling;

with et_keywords;				use et_keywords;
with et_module_names;


package body et_net_ports is



	function merge_ports (
		right, left : in type_net_ports)
		return type_net_ports
	is
		use pac_device_ports;
		use pac_net_submodule_ports;
		use et_netlists.pac_netchanger_ports;
		
		result : type_net_ports := left;

	begin
		union (result.devices, right.devices);
		union (result.submodules, right.submodules);
		union (result.netchangers, right.netchangers);
		
		return result;
	end merge_ports;



	
	procedure merge_ports (
		target	: in out type_net_ports;
		source	: in type_net_ports)
	is
		scratch : type_net_ports;
	begin
		scratch := merge_ports (target, source);
		target := scratch;
	end;

	
	

	function in_ports (
		ports	: in type_net_ports;
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
		ports	: in type_net_ports;
		port	: in type_net_submodule_port)
		return boolean
	is
		result : boolean := false;

		use pac_net_submodule_ports;
		port_cursor : pac_net_submodule_ports.cursor;
	begin
		port_cursor := ports.submodules.find (port);
		
		if has_element (port_cursor) then
			result := true;
		end if;

		return result;
	end in_ports;



	

	function no_ports (
		ports : in type_net_ports) 
		return boolean 
	is
		result : boolean := true;
		use pac_device_ports;
		use pac_net_submodule_ports;
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
		ports : in type_net_ports)
		return natural
	is 
		result : natural := 0;

		d, s, n : count_type := 0;

		use pac_device_ports;
		use pac_net_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		d := length (ports.devices);
		s := length (ports.submodules);
		n := length (ports.netchangers);

		result := natural (d + s + n);
		return result;
	end;

		



	function get_port_count_devices (
		ports : in type_net_ports)
		return natural
	is begin
		return natural (ports.devices.length);
	end;


	function get_port_count_submodules (
		ports : in type_net_ports)
		return natural
	is begin
		return natural (ports.submodules.length);
	end;

	
	function get_port_count_netchangers (
		ports : in type_net_ports)
		return natural
	is begin
		return natural (ports.netchangers.length);
	end;

	
	
end et_net_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
