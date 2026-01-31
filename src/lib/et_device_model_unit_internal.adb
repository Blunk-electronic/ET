------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     DEVICE MODEL / INTERNAL UNIT                         --
--                                                                          --
--                              B o d y                                     --
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

-- with et_exceptions;				use et_exceptions;


package body et_device_model_unit_internal is


	function get_placeholders (
		unit	: in type_unit_internal)
		return type_text_placeholders
	is begin
		if unit.appearance = APPEARANCE_PCB then
			return unit.symbol.placeholders;
		else
			return (others => <>);
		end if;
	end;



	function get_placeholders (
		unit	: in pac_units_internal.cursor)
		return type_text_placeholders
	is 
		u : type_unit_internal renames element (unit);
	begin
		return get_placeholders (u);
	end;





	function get_default_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in type_object_position)
		return type_text_placeholders
	is
		sym : type_unit_internal renames element (symbol_cursor);

		r : type_text_placeholders; -- to be returned
	begin
		r.name		:= sym.symbol.placeholders.name;
		r.value		:= sym.symbol.placeholders.value;
		r.purpose	:= sym.symbol.placeholders.purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate_placeholders (r, get_rotation (destination));
		
		return r;
	end get_default_placeholders;

	


	function get_symbol (
		unit	: in pac_units_internal.cursor)
		return type_symbol_model
	is
		u : type_unit_internal renames element (unit);
	begin
		return u.symbol;
	end;

	
		
	
	

	function get_ports_internal (
		unit_cursor	: in pac_units_internal.cursor)
		return pac_symbol_ports.map
	is begin
		return element (unit_cursor).symbol.ports;
	end;


	

	function get_port_positions (
		unit	: in pac_units_internal.cursor)
		return pac_points.list
	is
		result : pac_points.list;
	begin
		-- If the given cursor points to a unit, then
		-- extract the port positions. Otherwise return
		-- an empty list:		
		if has_element (unit) then
			result := get_port_positions (element (unit).symbol);
		end if;
		
		return result;
	end get_port_positions;

	
	
end et_device_model_unit_internal;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
