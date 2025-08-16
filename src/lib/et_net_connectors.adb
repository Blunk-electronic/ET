------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           NET CONNECTORS                                 --
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


with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;


package body et_net_connectors is

	
	function to_string (direction : in type_connector_direction) return string is begin
		return to_lower (type_connector_direction'image (direction));
	end;

	function to_direction (direction : in string) return type_connector_direction is begin
		return type_connector_direction'value (direction);
	end;




	function get_direction (
		connector	: in type_net_connector)
		return type_connector_direction
	is begin
		return connector.direction;
	end;


	function get_direction (
		connector	: in type_net_connector)
		return string
	is begin
		return to_string (get_direction (connector));
	end;


	

	

	procedure modify_status (
		connector 	: in out type_net_connector;
		operation	: in type_status_operation)
	is begin
		if connector.active then
			modify_status (connector.status, operation);
		end if;
	end;

	

	procedure reset_status (
		labels : in out type_net_connectors)
	is begin
		if labels.A.active then
			reset_status (labels.A.status);
		end if;
		
		if labels.B.active then
			reset_status (labels.B.status);
		end if;
	end;
	

	

	function is_active (
		connector : in type_net_connector)
		return boolean
	is begin
		return connector.active;
	end;
	

	procedure set_active (
		connector : in out type_net_connector)
	is begin
		connector := (active => true, others => <>);
	end;



	function is_proposed (
		connector : in type_net_connector)
		return boolean
	is begin
		if connector.active then
			return is_proposed (connector.status);
		else
			return false;
		end if;
	end;

	

	procedure set_proposed (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			set_proposed (connector.status);
		end if;
	end;


	procedure clear_proposed (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			clear_proposed (connector.status);
		end if;
	end;

	



	function is_selected (
		connector : in type_net_connector)
		return boolean
	is begin
		if connector.active then
			return is_selected (connector.status);
		else
			return false;
		end if;
	end;

	

	procedure set_selected (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			set_selected (connector.status);
		end if;
	end;


	procedure clear_selected (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			clear_selected (connector.status);
		end if;
	end;




	function is_moving (
		connector : in type_net_connector)
		return boolean
	is begin
		if connector.active then
			return is_moving (connector.status);
		else
			return false;
		end if;
	end;
	

	procedure set_moving (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			set_moving (connector.status);
		end if;
	end;


	procedure clear_moving (
		connector : in out type_net_connector)
	is begin
		if connector.active then
			clear_moving (connector.status);
		end if;
	end;
	
	
	
	procedure reset_connector (
		connector : in out type_net_connector)
	is begin
		connector := (active => false, others => <>);
	end;


	
	
end et_net_connectors;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
