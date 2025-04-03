------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                              DRILLS                                      --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;				use ada.text_io;


package body et_drills is

	procedure validate_drill_size (drill : in type_distance_model) is begin
		if drill not in type_drill_size then
			log (ERROR, "drill size invalid ! Allowed range is" 
				 & to_string (type_drill_size'first) & " .."
				 & to_string (type_drill_size'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_drill_size;


	
	function to_string (drill : in type_drill) return string is begin
		return ("C:" & to_string (drill.position) & " / D:" & to_string (drill.diameter));
	end to_string;

	

	function in_catch_zone (
		zone	: in type_catch_zone;
		drill	: in type_drill)
		return boolean
	is 
		c : type_circle;
	begin
		-- Build a circle from the given drll:
		set_center (c, drill.position);
		set_radius (c, drill.diameter * 0.5);

		-- Test whether the circle is in the catch zone:
		return in_catch_zone (zone, c); 
	end in_catch_zone;


	
	function is_selected (
		drill : in type_drill)
		return boolean
	is begin
		if drill.status.selected then
			return true;
		else
			return false;
		end if;
	end;
	

	procedure set_selected (
		drill : in out type_drill)
	is begin
		drill.status.selected := true;
	end;

	
	procedure clear_selected (
		drill : in out type_drill)
	is begin
		drill.status.selected := false;
	end;

	
	function is_proposed (
		drill : in type_drill)
		return boolean
	is begin
		if drill.status.proposed then
			return true;
		else
			return false;
		end if;
	end;
	

	
	procedure set_proposed (
		drill : in out type_drill)
	is begin
		drill.status.proposed := true;
	end;


	procedure clear_proposed (
		drill : in out type_drill)
	is begin
		drill.status.proposed := false;
	end;

	
	function is_moving (
		drill : in type_drill)
		return boolean
	is begin
		if drill.status.moving then
			return true;
		else
			return false;
		end if;
	end;
			

	procedure set_moving (
		drill : in out type_drill)
	is begin
		drill.status.moving := true;
	end set_moving;


	procedure clear_moving (
		drill : in out type_drill)
	is begin
		drill.status.moving := false;
	end clear_moving;


	
	procedure modify_status (
		drill 		: in out type_drill;
		operation	: in type_status_operation)
	is begin
		case operation.flag is
			when SELECTED =>
				case operation.action is
					when SET =>
						set_selected (drill);

					when CLEAR =>
						clear_selected (drill);
				end case;

				
			when PROPOSED =>
				case operation.action is
					when SET =>
						set_proposed (drill);

					when CLEAR =>
						clear_proposed (drill);
				end case;

				
			when MOVING =>
				case operation.action is
					when SET =>
						set_moving (drill);

					when CLEAR =>
						clear_moving (drill);
				end case;

				
			when others =>
				null; -- CS
		end case;
	end modify_status;

	

	
	procedure reset_status (
		drill : in out type_drill)
	is begin
		drill.status := (others => <>);
	end reset_status;

	
end et_drills;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
