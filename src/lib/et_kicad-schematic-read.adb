------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        KICAD READ SCHEMATIC                              --
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
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	

separate (et_kicad.schematic)

function read (
	current_schematic	: in type_hierarchic_sheet_file_name_and_timestamp;
	sheet_number		: in et_coordinates.type_sheet;
	log_threshold		: in type_log_level)
	return type_hierarchic_sheet_file_names_extended
is
	hierarchic_sheet_file_names : type_hierarchic_sheet_file_names_extended; -- list to be returned
	name_of_submodule_scratch : type_submodule_name.bounded_string; -- temporarily used before appended to hierarchic_sheet_file_names
	
	use pac_lines_of_file;
	
	line		: et_string_processing.type_fields_of_line; -- the line of the schematic file being processed
	lines		: pac_lines_of_file.list;
	line_cursor	: pac_lines_of_file.cursor;			

	sheet_file : type_schematic_file_name.bounded_string;

	net_id : natural := 0; -- for counting name-less nets (like N$1, N$2, N$3, ...)
	
	-- This is the total number of sheets as it is given in the sheet header. 
	-- A line like "Sheet 1 7" gives the sheet number (1), which is meaningless,
	-- and the total number of sheet of the design (7).
	sheet_count_total : type_sheet;

	wild_simple_labels	: type_simple_labels.list;
	wild_tag_labels 	: type_tag_labels.list;
	wild_segments		: type_wild_segments.list;
	wild_junctions		: type_junctions.list;

	-- In the first stage, all net segments of this sheet go into a wild collection of segments.
	-- Later they will be sorted and connected by their coordinates (start and and points)
	segment_count	: count_type := 0; -- holds the total number of segments within a sheet
	
	anonymous_strand : type_anonymous_strand;

	-- The list of anonymous strands. Procedure add_strand_to_anonymous_strands uses 
	-- this container for temporarily storage of anonymous strands.
	anonymous_strands : type_anonymous_strands.list; 

	procedure error_in_schematic_file (line : in type_fields_of_line) is
	begin
		log (ERROR, "in schematic file '" 
			& to_string (current_schematic.sheet.file) & "' " 
			& et_string_processing.affected_line (line)
			& to_string (line),
			console => true);
	end error_in_schematic_file;

	procedure add_segment_to_anonymous_strand (segment_cursor : in type_wild_segments.cursor) is
	-- Adds a net segment (indicated by given cursor) to anonymous_strand.
	-- This procedure happens to be called for a certain segment more than once (unavoidable). So the flag "picked" serves
	-- as indicator for a segment already added to the anonymous_strand.
		--scratch : type_net_segment_base;
		scratch : type_net_segment;

		procedure set_picked (segment : in out type_wild_net_segment ) 
		is begin 
			segment.picked := true;
		end set_picked;
		
	begin
		-- If segment already picked and added to anonymous_strand, do nothing with this segment. 
		-- Otherwise set the "picked" flag of that segment, output the coordinates of the segment, add it to anonymous net.
		if type_wild_segments.element (segment_cursor).picked then
			null;
			-- log (text => "  picked");
		else
			-- log (text => "  segment" & positive'image(id) & ":");
			-- log (text => "segment" & positive'image(id) & ":");
			-- log (text => "  segment" & positive'image(id) & ":");
			
			type_wild_segments.update_element (
				container	=> wild_segments,
				position	=> segment_cursor,
				process		=> set_picked'access);

-- 					write_coordinates_of_segment (segment => 
-- 						type_net_segment (type_wild_segments.element (segment_cursor)));
			
			log (text => to_string (
					segment	=> type_wild_segments.element (segment_cursor),
					scope	=> et_kicad_coordinates.XY),
					level => log_threshold + 1);

			scratch := type_net_segment (type_wild_segments.element (segment_cursor));
			type_net_segments.append (anonymous_strand.segments, scratch);
		end if;
	end add_segment_to_anonymous_strand;

	function search_for_same_coordinates (
	-- Starting from a segment indicated by id and the end point (given by side), 
	-- search in wild_segments for a segment with matching start or end point.
	-- In general untouched segments are preferred in the search. "Half" processed segments are of secondary relevance.
	-- Once a suitable segment was found, sc is assigned with neccessary data to be returned to the parent unit. The search for 
	-- a suitable segment excludes fully processed segments and the given segment (id).
		segment_cursor : in type_wild_segments.cursor;
		seg_in : in type_wild_net_segment;
		side : in type_segment_side) 
		return type_same_coord_result is

		sc : type_same_coord_result;
		line_start, line_end : et_kicad_coordinates.type_position;
		s, e : boolean; -- indicate the end point, that has been processed already
		untouched, half_processed : boolean; -- indicate whether a segment is completely untouched or processed in only one direction

		-- Prodcedures that set the s,e or picked flag in a wild net segment. 
		procedure set_e (segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
		procedure set_s (segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;
		
		use type_wild_segments;
		cursor : type_wild_segments.cursor;

	begin -- search_for_same_coordinates
		-- Set E/S flag:
		-- If we start the search from the end_point of a segment, the e-flag is to be set. This indicates the end_point has been processed.
		-- If we start the search from the start_point of a segment, the s-flag is to be set. This indicates the start_point has been processed.				
		case side is
			when end_point =>
				
-- 						log (text => "--> origin of search   (END): " 
-- 							 & type_grid'image(seg_in.coordinates_end.x) & "/" & type_grid'image(seg_in.coordinates_end.y),
-- 							 level => 1);
				
				type_wild_segments.update_element(
						container => wild_segments,
						position => segment_cursor,
						process => set_e'access);
				
			when start_point =>
				
-- 						log (text => "--> origin of search (START): " 
-- 							 & type_grid'image(seg_in.coordinates_start.x) & "/" & type_grid'image(seg_in.coordinates_start.y),
-- 							 level => 1);
				
				type_wild_segments.update_element(
						container => wild_segments,
						position => segment_cursor,
						process => set_s'access);
		end case;

		-- First, search completely untouched segments (they have both e and s flag cleared).
		-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
		-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
		-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
		cursor := wild_segments.first;
		while cursor /= type_wild_segments.no_element loop
			if cursor /= segment_cursor then -- skip the given segment
				line_start := type_wild_segments.element (cursor).coordinates_start;
				line_end   := type_wild_segments.element (cursor).coordinates_end;
				s  := type_wild_segments.element (cursor).s;
				e  := type_wild_segments.element (cursor).e;
				untouched := not (s or e); -- neither s nor e set

				if untouched then 
					--put(et_import.report_handle,"probe untouched segment: ");
					
					case side is
						-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
						when end_point =>
							--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
							if x (line_start) = x (seg_in.coordinates_end) and y (line_start) = y (seg_in.coordinates_end) then
								sc.valid := true;
								sc.side := start_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

							--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
							if x (line_end) = x (seg_in.coordinates_end) and y (line_end) = y (seg_in.coordinates_end) then
								sc.valid := true;
								sc.side := end_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

						-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.									
						when start_point =>
							--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
							if x (line_start) = x (seg_in.coordinates_start) and y (line_start) = y (seg_in.coordinates_start) then
								sc.valid := true;
								sc.side := start_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

							--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
							if x (line_end) = x (seg_in.coordinates_start) and y (line_end) = y (seg_in.coordinates_start) then
								sc.valid := true;
								sc.side := end_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;
					end case;
				end if;
			end if;

			next (cursor);
		end loop;

		-- No untouched segment found.
		-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
		-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
		-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
		-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
		cursor := wild_segments.first;
		while cursor /= type_wild_segments.no_element loop
			if cursor /= segment_cursor then -- skip the given segment
				line_start := type_wild_segments.element (cursor).coordinates_start;
				line_end   := type_wild_segments.element (cursor).coordinates_end;
				s  := type_wild_segments.element (cursor).s;
				e  := type_wild_segments.element (cursor).e;
				half_processed := s xor e;

				if half_processed then
					--put(et_import.report_handle,"probe half-processed segment: ");
					--write_coordinates_of_segment(type_net_segment(type_wild_segments.element(wild_segments,i)));
					
					case side is
						-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
						when end_point =>
							--if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
							if x (line_start) = x (seg_in.coordinates_end) and y (line_start) = y (seg_in.coordinates_end) then
								sc.valid := true;
								sc.side := start_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

							--if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
							if x (line_end) = x (seg_in.coordinates_end) and y (line_end) = y (seg_in.coordinates_end) then
								sc.valid := true;
								sc.side := end_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

						-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
						when start_point =>
							--if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
							if x (line_start) = x (seg_in.coordinates_start) and y (line_start) = y (seg_in.coordinates_start) then
								sc.valid := true;
								sc.side := start_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;

							--if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
							if x (line_end) = x (seg_in.coordinates_start) and y (line_end) = y (seg_in.coordinates_start) then
								sc.valid := true;
								sc.side := end_point;
								sc.cursor := cursor;
								goto matching_segment_coordinates_found;
							end if;
					end case;
				end if;
			end if;

			next (cursor);
		end loop;
		
		sc.valid := false;
		sc.cursor := cursor;
		return sc;
		
	<<matching_segment_coordinates_found>>
		add_segment_to_anonymous_strand (sc.cursor);
		--log (text => "match", level => 1);
		
		return sc;
	end search_for_same_coordinates;
	
	
	procedure associate_net_labels_with_anonymous_strands (log_threshold : in type_log_level) is
	-- All anonymous strands must be given a name. The name is enforced by net labels.
		
	-- The first label found on the strand dictates the strand name.
	-- Other labels on the strand are checked for their name only. 
	-- If the name differs from the strand name set earlier, an error is output.
	-- If scope of strands are contradicting, error is output.

	-- The kind of net label (simple, hierarchic, global) defines the scope of the strand.
	-- Net labels sitting on a segment, are added to the list of labels of that segment.
	
	-- Strands without label are named by using the notation "N$". 

		ls  	: type_net_label_simple;
		lt  	: type_net_label_tag;				
		anon_strand_a, anon_strand_b : type_anonymous_strand;
		--segment	: type_net_segment_base;
		segment	: type_net_segment;
		lls		: type_simple_labels.list;
		llt		: type_tag_labels.list;
	
		strand 		: type_strand;
		net_name	: pac_net_name.bounded_string;
		
		function label_sits_on_segment (
			label	: in type_net_label;
			segment	: in type_net_segment) return boolean is

			-- CS this is a workaround in order to provide a line for function line:
			type type_line_scratch is new et_symbols.pac_shapes.type_line with null record;
			line : type_line_scratch := (
				start_point	=> type_point (segment.coordinates_start), 
				end_point	=> type_point (segment.coordinates_end));
			
		begin
			return on_line (type_point (label.coordinates), line);
		end label_sits_on_segment;

		use type_net_segments;
		-- the segment cursor points to the segment being processed
		segment_cursor : type_net_segments.cursor; 

		use type_anonymous_strands;
		
		-- the strand cursor points to the anonymous strand being processed
		strand_cursor	: type_anonymous_strands.cursor := anonymous_strands.first;
		strand_cursor_b	: type_anonymous_strands.cursor;

		use type_simple_labels;
		simple_label_cursor	: type_simple_labels.cursor; -- points to the simple label being processed

		use type_tag_labels;
		tag_label_cursor	: type_tag_labels.cursor; -- points to the tag label being processed

		procedure output_net_label_conflict is begin
			log (ERROR, "Net label conflict !", console => true);
		end output_net_label_conflict;
		
	begin -- associate_net_labels_with_anonymous_strands
		log_indentation_up;
		
		-- This does only make sense if there are strands at all:
		if not is_empty (anonymous_strands) then
			log (text => "associating net labels with strands ...", level => log_threshold);
			
			-- Loop in list of anonymous strands, get a (non-processed-yet) strand, loop in list of segments and 
			-- find a (non-processed-yet) net label that sits on the net segment. If label sits on segment:
			--  - assume label text as name of strand (and check other labels of the anonymous strand)
			--  - set scope of strand according to the net label
			--
			--  - mark label as processed
			--  - update/replace label in wild_simple_labels or wild_tag_labels
			--
			--  - Mark anonymous strand as processed. This indicates that the strand has got a name (given by a label).
			--    Non-Processed strands are those without a label.
			--  - update/replace anonymous strand in anonymous_strands
			while strand_cursor /= type_anonymous_strands.no_element loop -- cursor already reset on declaration (see above)
				anon_strand_a := element (strand_cursor); -- get anonymous strand
				
				--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": "); -- CS: log ?
				if not anon_strand_a.processed then -- skip already processed nets

					-- reset segment cursor to begin of segment list of the anonymous net
					segment_cursor := anon_strand_a.segments.first;
					while segment_cursor /= type_net_segments.no_element loop -- loop for each segment in anonymous strand anon_strand_a
						segment := anon_strand_a.segments (segment_cursor);
						--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s); -- CS: log ?
						
						-- Loop in list of simple labels:
						if not is_empty (wild_simple_labels) then -- do it if there are simple labels at all
							--put_line(" simple labels ..."); -- CS: log ?
							
							simple_label_cursor := wild_simple_labels.first; -- reset label cursor
							while simple_label_cursor /= type_simple_labels.no_element loop
								ls := element (simple_label_cursor); -- get simple label
								
								if not ls.processed then
									--put(et_import.report_handle, "   probing "); write_coordinates_of_label( type_net_label(ls));  -- CS: log ?
									if label_sits_on_segment (label => type_net_label (ls), segment => segment) then

										if log_level >= log_threshold + 1 then
											log_indentation_up;
											--log (text => "label at" & to_string (label => type_net_label (ls), scope => xy));
											log (text => "label at" & to_string (label => type_net_label (ls)));
											log_indentation_down;
										end if;

										-- Check if the label does not contradict with other labels of this strand.
										-- Otherwise, set scope to local.
										case anon_strand_a.scope is
											when unknown => -- find. no label found so far. set scope of strand
												anon_strand_a.scope := local;

											when local => -- strand has been marked as "local" already. nothing to to
												null;

											when hierarchic => -- strand has been marked as "hierarchic" already. no local label allowed !
												output_net_label_conflict;
												log (ERROR,
													"hierarchic net " & to_string (anon_strand_a.name) 
													& " has a local label at" 
													--& to_string (position => ls.coordinates) & " !");
													& to_string (point => ls.coordinates) & " !");
												raise constraint_error;

											when global => -- strand has been marked as "global" already. no local label allowed !
												output_net_label_conflict;
												log (ERROR,
													"global net " & to_string (anon_strand_a.name) 
													& " has a local label at" 
													--& to_string (position => ls.coordinates) & " !");
													& to_string (point => ls.coordinates) & " !");
													raise constraint_error;
										end case;

										
										-- The first matching simple label dictates the strand name. 
										-- If other labels with text differing from strand name found, output warning.
										if pac_net_name.length (anon_strand_a.name) = 0 then -- If this is the first matching label

											-- assume the label text as strand name.
											anon_strand_a.name := ls.text; 
										else
											-- If label text is different from previously assigned strand name:
											if not pac_net_name."=" (anon_strand_a.name, ls.text) then
												output_net_label_conflict;

												-- for the log, some more information
												log (ERROR, 
														"Net " & to_string (anon_strand_a.name) & " has contradicting label " 
														--& "at" & to_string (position => ls.coordinates) & " !");
														& "at" & to_string (point => ls.coordinates) & " !");
												raise constraint_error;
											end if;
										end if;

										-- mark simple label as processed and update/replace it in wild_simple_labels
										ls.processed := true;
										type_simple_labels.replace_element (
											container => wild_simple_labels,
											position => simple_label_cursor,
											new_item => ls);

										-- Collect simple label (ls) in temporarily list of simple labels (lls).
										type_simple_labels.append (lls,ls);

										-- Mark anonymous strand as processed.
										anon_strand_a.processed := true;
									end if;
								end if;

								next (simple_label_cursor); -- advance label cursor
							end loop;

							-- Copy list of simple labels (lls) to current segment (s).
							segment.label_list_simple := lls;
							
							-- Update/replace segment in current anonymous strand.
							type_net_segments.replace_element (
								container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
								position => segment_cursor,
								new_item => segment); -- the updated segment
							
							-- Clean up: Purge temporarily list of simple labels for next spin.
							type_simple_labels.clear (lls);

							-- Update/replace anonymous net in anonymous_nets.
							type_anonymous_strands.replace_element (
								container => anonymous_strands, -- the list of anonymous strands
								position => strand_cursor,
								new_item => anon_strand_a); -- the updated anonymous net
						end if;
						
						-- Loop in list of tag labels:
						if not is_empty (wild_tag_labels) then -- do if if there are tag labels at all
							--put_line(" hierarchic and global labels ...");	 -- CS: log ?

							tag_label_cursor := wild_tag_labels.first; -- reset label cursor
							while tag_label_cursor /= type_tag_labels.no_element loop
								lt := element (tag_label_cursor); -- get tag label
								
								if not lt.processed then								
									if label_sits_on_segment (label => type_net_label (lt), segment => segment) then

										if log_level >= log_threshold + 1 then
											log_indentation_up;
											--log (text => "label at" & to_string (label => type_net_label (lt), scope => xy));
											log (text => "label at" & to_string (label => type_net_label (lt)));
											log_indentation_down;
										end if;

										-- Check if the label does not contradict with other labels of this strand.
										-- Otherwise, set scope according to the label just found.
										case anon_strand_a.scope is
											when unknown => -- find. no label found so far. set scope of strand
												if lt.global then 
													anon_strand_a.scope := global;
												end if;
												if lt.hierarchic then 
													anon_strand_a.scope := hierarchic;
												end if;

											when local => -- strand has been marked as "local" already. no hierarchic or global label allowed !
												if lt.global or lt.hierarchic then
													output_net_label_conflict;
													log (ERROR,
														"local net " & to_string (anon_strand_a.name) 
														& " has a hierarchic or global label at" 
														--& to_string (position => lt.coordinates) & " !");
														& to_string (point => lt.coordinates) & " !");
													raise constraint_error;
												end if;
												
											when hierarchic => -- strand has been marked as "hierarchic" already. no global label allowed !
												if lt.global then
													output_net_label_conflict;
													log (ERROR,
														"hierarchic net " & to_string (anon_strand_a.name) 
														& " has a global label at" 
														--& to_string (position => lt.coordinates) & " !");
														& to_string (point => lt.coordinates) & " !");
													raise constraint_error;
												end if;

											when global => -- strand has been marked as "global" already. no hierarchic label allowed !
												if lt.hierarchic then
													output_net_label_conflict;
													log (ERROR,
														"global net " & to_string (anon_strand_a.name) 
														& " has a hierarchic label at" 
														--& to_string (position => lt.coordinates) & " !");
														& to_string (point => lt.coordinates) & " !");
													raise constraint_error;
												end if;
										end case;

										-- The first matching label dictates the net name and scope. 
										-- If other labels with text differing from net name found, output warning.
										if length (anon_strand_a.name) = 0 then -- If this is the first matching label
											anon_strand_a.name := lt.text; -- assume the label text as net name.
										else
											-- If label text is different from previously assigned net name:
											if anon_strand_a.name /= lt.text then 
												log (ERROR, 
														"Net " & to_string (anon_strand_a.name) & " has contradicting label " 
														--& "at" & to_string (position => lt.coordinates) & " !");
														& "at" & to_string (point => lt.coordinates) & " !");
												raise constraint_error;
											end if;
										end if;

										-- mark tag label as processed and update/replace it in wild_tag_labels
										lt.processed := true;
										type_tag_labels.replace_element (
											container => wild_tag_labels,
											position => tag_label_cursor,
											new_item => lt);

										-- Collect tag label (lt) in temporarily list of simple labels (llt).
										type_tag_labels.append (llt,lt);

										-- Mark anonymous net as processed.												
										anon_strand_a.processed := true;
									end if;
								end if;

								next (tag_label_cursor);
							end loop;

							-- Copy list of tag labels (llt) to current segment (s).
							segment.label_list_tag := llt;
							
							-- Update/replace segment in current anonymous net.
							type_net_segments.replace_element (
								container => anon_strand_a.segments, -- the list of segments of the current anonymous strand
								position => segment_cursor,
								new_item => segment); -- the updated segment

							-- Clean up: Purge temporarily list of tag labels for next spin.
							type_tag_labels.clear (llt);

							-- Update/replace anonymous net in anonymous_nets.
							type_anonymous_strands.replace_element (
								container => anonymous_strands, -- the list of anonymous strands
								position => strand_cursor,
								new_item => anon_strand_a); -- the updated anonymous net
						end if;

						next (segment_cursor); -- advance segment cursor
					end loop;
				end if;

				next (strand_cursor); -- advance strand cursor
			end loop;

			-- Build name-less strands from anonymous strands.
			-- Anonymous strands have no label, hence no name -> "processed" flag is still cleared.
			-- As placeholder for the name we use the notation "N$n" where n is taken from the net_id (counter of name-less strands)
			-- Their scope is strictly "local".
			--
			-- We build a new strand (of type type_strand_named) in an intermediate variable "strand"
			-- (as specified in et_schematic type_strand_named) for transfer to the module netlist.
			--
			-- NOTE: Even if a strand has no name at this stage, it may get a dedicated name later.
			-- Power-out ports may overwrite the strand name.
			log (text => "building name-less strands ...", level => log_threshold);
			log_indentation_up;

			strand_cursor := anonymous_strands.first; -- reset strand cursor
			while strand_cursor /= type_anonymous_strands.no_element loop
				anon_strand_a := element (strand_cursor);  -- get anonymous strand

				if not anon_strand_a.processed then

					-- build temporarily strand with a name like N$542
					net_id := net_id + 1; -- increment net id. net_id applies for the whole design. see declarations of procedure import_design
					net_name := to_net_name (
						anonymous_net_name_prefix & trim (natural'image (net_id), left));

					log (text => to_string (net_name), level => 2);
					
					strand.name := net_name;
					strand.scope := local;

					log_indentation_up;
					log (text => "scope " & to_string (strand.scope) & " with segments", level => 2);
					
					-- fetch net segments from anonymous strand and append them to the new name-less strand:
					segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous net
					while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous strand anon_strand_a
						segment := element (segment_cursor); -- get segment
						type_net_segments.append (container => strand.segments, new_item => segment);
						
						if log_level >= 2 then
							--write_coordinates_of_segment (segment => segment);
							log_indentation_up;
							log (text => to_string (segment => segment, scope => xy));
							log_indentation_down;
						end if;
						
						next (segment_cursor);
					end loop;

					log_indentation_down;
					
					-- assign coordinates
					set_path (strand.position, path_to_sheet);
					set_sheet (strand.position, sheet_number);

					-- insert strand in module, then purge strand.segments for next spin
					log (text => "inserting strand in module ...", level => log_threshold + 2);
					add_strand (strand);

					type_net_segments.clear (strand.segments);
				end if;

				next (strand_cursor); -- advance strand cursor
			end loop;
			
			log_indentation_down;
			
			-- Build named strands with label. Those strands have the "processed" flag set.
			-- NOTE: Even if a strand has a dedicated name at this stage, it may get a dedicated name later on netlist generation.
			-- Power-out ports may overwrite the strand name (which would be regarded as design error and is handled on netlist generation)
			log (text => "building named strands ...", level => log_threshold);
			log_indentation_up;
			
			strand_cursor := anonymous_strands.first; -- reset strand cursor
			while strand_cursor /= type_anonymous_strands.no_element loop
				anon_strand_a := element (strand_cursor);  -- get a strand

				if anon_strand_a.processed then -- it must have a name

					log (text => to_string (anon_strand_a.name), level => 2);
					
					strand.name := anon_strand_a.name;
					strand.scope := anon_strand_a.scope;

					log_indentation_up;
					log (text => "scope " & to_string (strand.scope) & " with segments", level => 2);

					-- fetch net segments from anonymous strand and append them to the new named strand:
					segment_cursor := anon_strand_a.segments.first; -- reset segment cursor to begin of segments of the current anonymous strand
					while segment_cursor /= type_net_segments.no_element loop -- loop for each segment of anonymous_strand "a"
						segment := element (segment_cursor); -- get segment
						type_net_segments.append (container => strand.segments, new_item => segment);
						
						if log_level >= 2 then
							--write_coordinates_of_segment (segment => segment);
							log (text => to_string (segment => segment, scope => xy));
						end if;
						
						next (segment_cursor);
					end loop;

					log_indentation_down;

					-- assign coordinates
					set_path (strand.position, path_to_sheet);
					set_sheet (strand.position, sheet_number);

					-- insert strand in module, then purge strand.segments for next spin
					log (text => "inserting strand in module ...", level => log_threshold + 2);
					add_strand (strand);
					type_net_segments.clear (strand.segments);

				end if;

				next (strand_cursor); -- advance strand cursor
			end loop;

			log_indentation_down;
			
		else
			log (NOTE,
					"The schematic does not contain nets to associate net labels with !");
		end if;

		log_indentation_down;
	end associate_net_labels_with_anonymous_strands;
	
	procedure process_junctions (log_threshold : in type_log_level) is
	-- Breaks down all net segments where a junction sits on. 
	-- In the end, the number of net segments may increase.

	-- NOTE: The junction to be tested is taken from the wild list of net junctions. This
	-- list contains the junction of the current sheet exclusively.
	
	-- Loops in wild_segments and tests if a junction sits on a segment.
	-- Then splits the segment where the junction sits. If there are junctions left on 
	-- the remaining fragments, they will be detected in the next spin. 
	-- The flag segment_smashed indicates there are no more segments left with a junction.
		segment : type_wild_net_segment;
		junction : type_net_junction;
	
		use type_junctions;
		junction_cursor : type_junctions.cursor; -- points to the junction being processed

		procedure change_segment_start_coordinates (segment : in out type_wild_net_segment) is 
		begin
			segment.coordinates_start := junction.coordinates;
		end change_segment_start_coordinates;
		
		segment_smashed : boolean := true; -- indicates whether a segment has been broken down

		use type_wild_segments;
		segment_cursor : type_wild_segments.cursor; -- points to the current segment
		
	begin -- process_junctions
		log_indentation_up;
		
		-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
		-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
		-- CS NOTE: In this process, segments may evolve, which have junctions not sitting at the segment. A clean up would be useful.
		if not is_empty (wild_junctions) then 
			log (text => "processing" & count_type'image (length (wild_junctions)) & " net junctions ...", level => log_threshold);
			log_indentation_up;
			
			-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
			-- does not increase anymore, all segments are processed.
			while segment_smashed loop
				
				segment_cursor := wild_segments.first;
				loop_s:
				while segment_cursor /= type_wild_segments.no_element loop
				
					segment := type_wild_segments.element (segment_cursor); -- get a segment
					log (text => "probing segment" & to_string (segment => segment, scope => xy), level => log_threshold);

					-- loop in wild junction list until a junction has been found that sits on the segment
					junction_cursor := wild_junctions.first; -- reset junction cursor to begin of junction list
					while junction_cursor /= type_junctions.no_element loop

						-- fetch junction from current cursor position
						junction := type_junctions.element (junction_cursor);
						
						if junction_sits_on_segment (junction, type_net_segment_base (segment)) then -- match

							if log_level >= log_threshold + 1 then
								log_indentation_up;
								log (text => "has junction" & to_string (position => junction.coordinates, scope => xy));
								log_indentation_down;
							end if;
							-- NOTE: junctions sitting on a net crossing may appear twice here.

							-- move start coord. of the current segment to the position of the junction
							type_wild_segments.update_element (
								container	=> wild_segments,
								position	=> segment_cursor,
								process		=> change_segment_start_coordinates'access
								);

							-- replace end coord. of segment by pos. of junction
							segment.coordinates_end := junction.coordinates;

							-- If the junction has not been appended to the segment yet, 
							-- append junction to the segment:
							-- NOTE: junctions may be appended twice if they sit on net crossings.
							-- For this reason we first test if the junction has already been appended.
							if not type_junctions.contains (segment.junctions, junction) then
								type_junctions.append (
									container	=> segment.junctions,
									new_item	=> junction);
							end if;
							
							-- append new segment to list of wild segments
							type_wild_segments.append (
								container	=> wild_segments,
								new_item	=> segment
								);

							exit loop_s;
						end if;

						next (junction_cursor);
					end loop;

					next (segment_cursor);
				end loop loop_s;

				-- Test if segment_count has increased. If yes, set segment_smashed flag so that the wild_segments
				-- can be searched again. Otherwise clear segment_smashed -> end of procedure.
				if type_wild_segments.length (wild_segments) > segment_count then
					segment_smashed := true;
					-- update segment_count (should increment by 1)
					segment_count := type_wild_segments.length (wild_segments);
				else
					segment_smashed := false;							
				end if;
			end loop;

			log_indentation_down;
			log (text => "update: net segments total" & count_type'image (segment_count), level => log_threshold);
		end if;

		log_indentation_down;
	end process_junctions;


	procedure build_anonymous_strands (log_threshold : in type_log_level) is
	-- From the wild segments and junctions assemble net segments to anonymous strands.

		procedure add_strand_to_anonymous_strands is
		-- Once an anonymous strand is complete, it gets appended to a list of anonymous strands. 
		-- Afterward the anonymous strand is deleted. It is a list of net segments which must be purged so that the list
		-- "anonymous_strand" can be filled with net segments of the next anonymous strand.
		begin
			type_anonymous_strands.append (anonymous_strands, anonymous_strand);
			type_net_segments.clear (anonymous_strand.segments);
		end add_strand_to_anonymous_strands;

		use type_wild_segments;

		-- primary and secondary segment cursors.
		segment_cursor_a, segment_cursor_b : type_wild_segments.cursor;

		-- node of the segment (end or start point)
		side : type_segment_side;

		-- the result of a segment search
		search_result : type_same_coord_result;
		
	begin -- build_anonymous_strands
		log_indentation_up;
		
		-- Build anonymous nets:
		-- We are processing the net segments of a sheet here. The net segments have been collected in 
		-- a wild collection of net segments earlier.
		-- This wild collection of segments does not reveal the actual nets where the segments belong to.
		-- The segments are inspected
		-- in the following by looking at the coordinates of their start and end points. 
		-- Segments whose start or end points match other segments are considered
		-- as connected to each other (means they belong to the same strand).
		-- The net name is unknown yet. So the outcome of the following is a list of anonymous strands.
		
		-- CS: handle circlular strands, currently they cause a forever-loop here
		
		segment_count := type_wild_segments.length (wild_segments); -- get number of segments on the current sheet

		log (text => "processing" & count_type'image (segment_count) & " net segments ...", level => log_threshold);

		-- It may happen that a sheet has no nets, for example the top level sheet of a design.
		-- If there are no net segments at all, nothing happens.
		if segment_count > 0 then 

			-- Segments where a junction sits on, must be broken down. This results in more segments than calculated earlier.
			-- The outcome of process_junctions might be a greater number of net segments than currently being held in segment_count.
			process_junctions (log_threshold + 1);
			-- segment_count now has been updated

			log_indentation_up;
			
			-- We inspect one segment after another. segment_cursor_a points to the first segment to be processed. 
			-- A segment, whose "e" AND "s" flag has been set, is to be skipped (because this segment has been processed already).
			-- Variable side_scratch points to the side of the segment (start or end point) where another matching segment
			-- is to be searched for.
			-- If a matching segment is found, it gets appended to the current anonymous strand.

			-- set primary segment cursor to begin of wild segment collection
			segment_cursor_a := wild_segments.first; 

			-- The primary segment cursor advances once an anonymous stand is complete (when all connected segments have been found).
			-- Each time a connected segment has been found, the secondary segment cursor points to that segment.
			while segment_cursor_a /= type_wild_segments.no_element loop
				segment_cursor_b := segment_cursor_a;

				-- Already processed segments are skipped. (Processed segments have the "s" and "e" flag set.)
				if not type_wild_segments.element (segment_cursor_b).s and 
					not type_wild_segments.element (segment_cursor_b).e then 

					-- We initiate a new strand and start looking for a matching segment on the end_point:
					--put_line(et_import.report_handle," anonymous net" & positive'image(seg) & ":"); 
					log (text => "assembling strand with segments", level => log_threshold + 1);
					log_indentation_up;

					-- The first segment is to be added to the anonymous strand.
					add_segment_to_anonymous_strand (segment_cursor_b); 
					side := end_point;

					loop -- A
						--put_line(et_import.report_handle,"  --> A"); -- CS: log ?
						
						-- Search for a segment connected to the current segment. 
						-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment
						-- to current anonymous strand.
						-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to indicate
						-- which end point has been processed.
						-- If no connected segment found, toggle side and repeat search_for_same_coordinates
						-- on the opposide of the segment.
						search_result := search_for_same_coordinates (
							segment_cursor	=> segment_cursor_b,
							seg_in			=> type_wild_segments.element (segment_cursor_b),
							side			=> side);

						if search_result.valid then
							--put_line(et_import.report_handle,"  --> E"); -- CS: log ?
							null;
						else
							-- Toggle side_scratch depending on the e/s flag of the segment:
							-- D
							if type_wild_segments.element (segment_cursor_b).e then
								-- put_line(et_import.report_handle,"  --> D1"); -- CS: log ?
								side := start_point;
							end if;
							
							if type_wild_segments.element (segment_cursor_b).s then
								-- put_line(et_import.report_handle,"  --> D2"); -- CS: log ?
								side := end_point;	
							end if;

							-- C
							--put_line(et_import.report_handle,"  --> C"); -- CS: log ?
							
							-- Search for a segment connected to the current side of the segment.
							-- If function search_for_same_coordinates discovers a suitable segment, 
							-- it adds the segment to current anonymous strand.
							-- Search_for_same_coordinates sets the "e" or "s" flag of the segment in order to
							-- indicate which end point has been processed.
							-- If no connected segment found, the current anonymous net is considered
							-- as complete -> cancel loop, advance to next segment ...
							search_result := search_for_same_coordinates (
								segment_cursor	=> segment_cursor_b,
								seg_in			=> type_wild_segments.element (segment_cursor_b),
								side			=> side);
							
							if search_result.valid then
								--put_line(et_import.report_handle,"  --> F"); -- CS: log ?
								null;
							else
								--put_line(et_import.report_handle,"  done"); -- CS: log ?
								
								-- All collected segments belong to the same net.
								-- This net is to be added to the list of anonymous nets.
								add_strand_to_anonymous_strands; 	
																	
								exit;	-- no further segment search required.
							end if;
						end if;

						-- B
						--put_line(et_import.report_handle,"  --> B"); -- CS: log ?
						
						-- Update secondary segment_cursor with the cursor of the segment just found 
						-- by search_for_same_coordinates. So the secondary segment cursor now points to the next
						-- connected segment. 
						-- Same_coord_result contains the end point of the segment that has just been found.
						-- Depending on the end point of the matching segment, side must be set so that the
						-- search can continue on the opposide of the new segment.
						segment_cursor_b := search_result.cursor;
						
						case search_result.side is
							when end_point => 
								side := start_point;
							when start_point =>
								side := end_point;
						end case;
					end loop;

					log_indentation_down;
				end if;

				-- advance primary segment cursor
				next (segment_cursor_a);
			end loop;

			log_indentation_down;

		end if;

		log_indentation_down;

	end build_anonymous_strands;


	procedure check_header (line : in type_fields_of_line) is
	-- Tests the given line if it contains a valid schematic sheet header.
	-- Sets the flag schematic_headline_processed.
	-- Aborts program if schematic version invalid.
		use et_import;
	begin
		if f (line,1) = schematic_header_keyword_sys_name and
			f (line,2) = schematic_header_keyword_schematic and
			f (line,3) = schematic_header_keyword_file and
			f (line,4) = schematic_header_keyword_version then
				case cad_format is
					when KICAD_V4 =>
						if positive'value (f (line,5)) = schematic_version_v4 then
							-- headline ok, version is supported
							schematic_version_valid := true;
						else
							log (ERROR, "schematic version" 
									& positive'image (schematic_version_v4) & " required.",
								console => true);
							raise constraint_error;
						end if;

					when KICAD_V5 =>
						if positive'value (f (line,5)) = schematic_version_v5 then
							-- CS: currently the version number must exactly match. Range check ?
							-- headline ok, version is supported
							schematic_version_valid := true;
						else
							log (ERROR, "schematic version" 
									& positive'image(schematic_version_v5) & " required.",
								console => true);
							raise constraint_error;
						end if;

					when others => raise constraint_error;
						
				end case;
		end if;
	end check_header;
	
	procedure make_sheet_header (lines : in pac_lines_of_file.list) is
	-- Builds the sheet header.
	-- The sheet header mainly contains the used libraries.

		sheet_header : type_sheet_header; -- the header being built
	
		--	LIBS:nucleo_core-rescue
		--	LIBS:power
		-- 	LIBS:bel_connectors_and_jumpers
		--	LIBS:bel_primitives
		--	LIBS:bel_stm32
		--	LIBS:nucleo_core-cache
		--	EELAYER 25 0
		--	EELAYER END

		-- This data goes into a the sheet_header. When the schematic file has been
		-- read completely, the sheet_header is appended to global list_of_sheet_headers. 
		-- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.
		-- NOTE: The library entries in the header are not used by kicad. However, they must be read
		-- and stored in sheet_header.libraries.
						
	begin -- make_sheet_header
		line_cursor := first (lines);
		while line_cursor /= pac_lines_of_file.no_element loop

			--log ("---> C " & to_string (line));
			
			-- Field #1 of the line must be broken down by its own ifs in order to get "LIBS" and "bel_stm32"
			if get_field_from_line (f (element (line_cursor), 1), 1, latin_1.colon) = schematic_library then

				-- for the log: write library name
				log (text => "uses library " & get_field_from_line (f (element (line_cursor), 1), 2, latin_1.colon),
					level => log_threshold + 1);

				-- Store bare library name in the list sheet_header.libraries:
				-- We use a doubly linked list because the order of the library names must be kept.
				type_library_names.append (
					container	=> sheet_header.libraries,
					new_item	=> et_kicad_general.to_library_name (
						get_field_from_line (f (element (line_cursor), 1), 2, latin_1.colon))
					);

			end if;

			-- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
			-- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
			if f (element (line_cursor), 1) = schematic_eelayer then
				if f (element (line_cursor), 2) = schematic_eelayer_end then
					null;
				else
					-- append layer numbers to the sheet header
					sheet_header.eelayer_a := positive'value(
						f (element (line_cursor), 2));

					sheet_header.eelayer_b := natural'value(
						f (element (line_cursor), 3));
				end if;
			end if;

			next (line_cursor);
		end loop;

		-- Add sheet_header to module.
		-- NOTE: The file name serves as key in order to map from file to header.
		add_sheet_header (
			header	=> sheet_header,
			sheet	=> current_schematic.sheet.file);

	end make_sheet_header;

	procedure make_drawing_frame (
	-- Builds the drawing frame.
	-- CS: Read lines and position of text placeholders from
	-- *.kicad_wks file (either the default file or the one specified
	-- in the project file by a line like "PageLayoutDescrFile=/home/user/tmp/sheet.kicad_wks".
		lines 			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is

		use et_frames;
		
		frame : type_frame; -- a single drawing frame. see type in et_kicad.ads
	
		-- If the description reveals that there is more than one sheet, we have a hierarchic design. Means we
		-- need to read follwing sheet sections.
		-- The sheet_number_current obtained here serves as part of the coordinates of objects found on this sheet.
		-- The sheet description looks like this:

		-- $Descr A4 11693 8268
		-- encoding utf-8
		-- Sheet 5 8
		-- Title ""
		-- Date ""
		-- Rev ""
		-- Comp ""
		-- Comment1 ""
		-- Comment2 ""
		-- Comment3 ""
		-- Comment4 ""
		-- $EndDescr

		-- NOTE: The problem with the attributes title, date, rev, comp is that they apply for individual sheets and
		-- are thus not project wide.
		-- CS: These attributes are currently lost during conversion.
		
	begin
		log (text => "making drawing frame ...", level => log_threshold);
		log_indentation_up;
	
		line_cursor := first (lines);

		-- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
		-- CS test field count
		frame.paper := to_paper_size (f (element (line_cursor), 2));

		-- The sheet size seems to be ignored by kicad. Only the paper_size matters.
-- 				frame.size_x		:= mil_to_distance (f (element (line_cursor), 3)); 
-- 				frame.size_y 		:= mil_to_distance (f (element (line_cursor), 4)); 
		
		--frame.coordinates.path := path_to_submodule;
		set_path (frame.coordinates, path_to_sheet);

		-- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
		-- kicad built-in things and remain unassigned here.

		next (line_cursor);

		-- read endcoding from a line like "encoding utf-8"
		-- CS: checks only for a non-default endcoding and outputs a warning.
		-- CS: we assume only one encoding. other encodings are ignored currently.
		-- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
		-- good idea.
		if f (element (line_cursor), 1) = schematic_keyword_encoding then
			-- CS test field count
			if f (element (line_cursor), 2) /= encoding_default then
				log (WARNING, "non-default endcoding '" 
						& f (element (line_cursor), 2) & "' found !");
			end if;
		end if;

		next (line_cursor);

		-- Log sheet number on encountering a line like "Sheet 1 7"
		-- NOTE: The sheet number written here (field 2) has no meaning. The real sheet number is 
		-- obtained by reading the value of sheet_number. sheet_number is has been incremented
		-- before function read_schematic was called.
		if f (element (line_cursor), 1) = schematic_keyword_sheet then
			-- CS test field count

			-- The sheet number written here is meaningless:
			--sheet_number_current := to_sheet_number (field (element (line_cursor), 2));
			-- Instead we log the global sheet_number:
			log (text => "sheet number" & to_sheet (sheet_number), level => log_threshold + 1);

			-- Get the total number of sheet of this design. 
			sheet_count_total := to_sheet (f (element (line_cursor), 3));
			
			-- CS: sheet_count_total must not change from sheet to sheet. Check required.
			if sheet_count_total > 1 then
				-- Set in the hierarchic_sheet_file_names (to be returned) the parent_sheet name. The schematic file 
				-- being processed (see input parameters of read_schematic) becomes the parent sheet
				-- of the sheet here.
				hierarchic_sheet_file_names.parent_sheet := to_submodule_name (
					to_string (current_schematic.sheet.file));
			end if;
			-- CS: make sure total sheet count is less or equal current sheet number.

			-- Our temporarily drawing frame gets the current sheet number assigned.
			set_sheet (frame.coordinates, sheet_number);
		end if;						

		next (line_cursor);

		-- read sheet title from a line like "Title "abc""
		if f (element (line_cursor), 1) = schematic_keyword_title then
			log (text => "sheet title", level => log_threshold + 1);
			
			-- CS test field count
			-- x := to_content ((f (element (line_cursor), 2)));
			
			-- CS set sheet specific description
		end if;

		next (line_cursor);
		
		-- read date from a line like "Date "1981-01-23""
		if f (element (line_cursor), 1) = schematic_keyword_date then
			log (text => "sheet date", level => log_threshold + 1);
			
			-- CS test field count					
			-- x := to_content (f (element (line_cursor), 2));

			-- CS What shall we do with the sheet date ? Since we do a conversion from kicad to ET
			-- this information seems to get lost anyway.
		end if;

		next (line_cursor);
		
		-- read revision from a line like "Rev "9.7.1"
		if f (element (line_cursor), 1) = schematic_keyword_revision then
			log (text => "sheet revision", level => log_threshold + 1);
			
			-- CS test field count					
			-- x := to_content (f (element (line_cursor), 2));
			
			-- CS set the module revision
		end if;

		next (line_cursor);

		-- read company name
		if f (element (line_cursor), 1) = schematic_keyword_company then
			log (text => "sheet company name", level => log_threshold + 1);
			
			-- CS test field count					
			-- x := to_content (f (element (line_cursor), 2));

			-- CS set the module company name
		end if;

		next (line_cursor);

		-- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
		if  f (element (line_cursor), 1) = schematic_keyword_comment_1 or
			f (element (line_cursor), 1) = schematic_keyword_comment_2 or
			f (element (line_cursor), 1) = schematic_keyword_comment_3 or 
			f (element (line_cursor), 1) = schematic_keyword_comment_4 then

			log (text => "sheet comment", level => log_threshold + 1);
			
			-- CS test field count
			-- x := to_content (f (element (line_cursor), 2));

			-- CS set the sheet specific comment
		end if;


		-- CS: x/y coordinates and list of lines of a title block are kicad built-in things and 
		-- thus not available here. -> x/y assume default values (0/0).
		-- See comment above in header of this procedure.


		-- append temporarily drawing frame to module
		add_frame (frame);

		log_indentation_down;

		exception
			when event:
				others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_message (event));
					raise;
		
	end make_drawing_frame;


	procedure make_gui_sheet (
	-- Builds the hierachic sheet.
		lines 			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is

		sheet		: type_hierarchic_sheet; -- the hierarchical sheet being built
		sheet_name	: type_hierarchic_sheet_name; -- incl. file name and sheet name

		port_inserted	: boolean; -- used to detect multiple ports with the same name
		port_cursor		: type_hierarchic_sheet_ports.cursor; -- obligatory, but not read

		use type_submodule_name;
		use et_conventions;

		text_size : et_kicad_libraries.pac_text.type_text_size; -- temporarily storage of a text size before being checked
	
		function to_direction (dir_in : in string) return type_port_direction is
		-- Converts a string to type_port_direction.
			result : type_port_direction;
			dir : type_sheet_port_direction; -- see et_kicad.ads
		begin
			dir := type_sheet_port_direction'value (dir_in);
			
			case dir is
				when I => result := INPUT;
				when O => result := OUTPUT;
				when B => result := BIDIR;
				when T => result := TRISTATE;
				when U => result := PASSIVE;
			end case;

			log_indentation_up;
			log (text => to_string (result), level => log_threshold + 2);
			log_indentation_down;
			
			return result;

			exception
				when constraint_error =>
					log (ERROR, "invalid port direction '" 
							& dir_in & "' !");
					-- CS: provide more details
					raise;

		end to_direction;

		function to_orientation (or_in : in string) return et_coordinates.type_rotation is
		-- Converts a string to type_rotation
			result : et_coordinates.type_rotation;
			orientation : type_sheet_port_orientation; -- see et_kicad.ads
		begin
			orientation := type_sheet_port_orientation'value (or_in);

			case orientation is
				when R => result := et_coordinates.type_rotation (0.0);
				when L => result := et_coordinates.type_rotation (180.0);
			end case;
			
			return result;

			exception
				when constraint_error =>
					log (ERROR, "invalid port orientation '" 
							& or_in & "' !");
					-- CS: provide more details
					raise;

		end to_orientation;
			
	begin -- make_gui_sheet
		log (text => "making gui sheet ...", level => log_threshold);
		log_indentation_up;
		
		line_cursor := pac_lines_of_file.first (lines);
-- 				log (text => to_string (line), level => log_threshold + 1);

		-- read GUI sheet position and size from a line like "S 4050 5750 1050 650"
		if f (element (line_cursor), 1) = schematic_keyword_sheet_pos_and_size then
			-- CS test field count
			set_path (sheet.coordinates, path_to_sheet);
			--log (text => "path " & to_string (path (sheet.coordinates)));
			set_sheet (sheet.coordinates, sheet_number);
			
			set (X, mil_to_distance (f (element (line_cursor), 2)), sheet.coordinates);
			set (Y, mil_to_distance (f (element (line_cursor), 3)), sheet.coordinates);

			sheet.size_x := mil_to_distance (f (element (line_cursor), 4));
			sheet.size_y := mil_to_distance (f (element (line_cursor), 5));                                
		end if;

		next (line_cursor);
-- 				log (text => to_string (line), level => log_threshold + 1);
		
		-- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
		if f (element (line_cursor), 1) = schematic_keyword_sheet_timestamp then 
			-- CS test field count					
			sheet.timestamp := type_timestamp (f (element (line_cursor), 2));
		end if;

		next (line_cursor);
		
		-- Read sheet name from a line like "F0 "mcu_stm32f030" 60"
		if f (element (line_cursor), 1) = schematic_keyword_sheet_name then
			-- CS test field count					
			sheet_name.name := to_submodule_name (f (element (line_cursor), 2));

			-- set text size of sheet name and test for excessive text size.
			sheet.text_size_of_name := et_kicad_libraries.pac_text.to_text_size (mil_to_distance (f (element (line_cursor), 3)));

			-- Test text size by category.
			check_schematic_text_size (category => et_conventions.SHEET_NAME, size => sheet.text_size_of_name);
		end if;

		next (line_cursor);
		
		-- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
		if f (element (line_cursor), 1) = schematic_keyword_sheet_file then
			-- CS test field count					
			sheet_name.file := to_schematic_file_name (f (element (line_cursor), 2));
			
			-- set text size of file name and test for excessive text size
			sheet.text_size_of_file := et_kicad_libraries.pac_text.to_text_size (mil_to_distance (f (element (line_cursor), 3)));

			-- Test text size by category.
			check_schematic_text_size (category => FILE_NAME, size => sheet.text_size_of_file);
			
			-- Append sheet file name to hierarchic_sheet_file_names. 
			-- This list will be returned by this function (we are in read_schematic) to the calling
			-- parent unit (import_design).
			type_hierarchic_sheet_file_names.append (
				container	=> hierarchic_sheet_file_names.sheets,
				new_item	=> (
								sheet		=> sheet_name, -- incl. file name and sheet name
								timestamp	=> sheet.timestamp)); -- B5D45A33
		end if;

		log (text => "hierarchic sheet " & to_string (submodule => sheet_name.name), level => log_threshold + 1);
		
		-- Read sheet ports from a line like "F2 "SENSOR_GND" I R 2250 3100 60".
		-- The index after the F is a successive number that increments on every port:
		-- So the next port would be "F3 "SENSOR_VCC" I R 2250 3300 60" ...
		next (line_cursor);

		-- Read ports of hierachic sheet if any. Otherwise output a warning.
		-- If no ports available, the line cursor points to a no_element.
		if line_cursor /= pac_lines_of_file.no_element then
			
			-- Test of excessive text size.
			text_size := et_kicad_libraries.pac_text.to_text_size (mil_to_distance (f (element (line_cursor),  7)));

			-- Test text size by category.
			check_schematic_text_size (category => PORT_NAME, size => text_size);
			
			while line_cursor /= pac_lines_of_file.no_element loop
				log_indentation_up;
				log (text => "port " & strip_quotes (f (element (line_cursor),  2)), level => log_threshold + 2);

				-- add port
				type_hierarchic_sheet_ports.insert (
					container => sheet.ports,
					key => to_net_name (f (element (line_cursor),  2)), -- port name
					new_item => (
						direction 	=> to_direction (f (element (line_cursor),  3)),
						orientation	=> to_orientation (f (element (line_cursor),  4)),
						coordinates	=> to_point (f (element (line_cursor),  5), f (element (line_cursor),  6)),
						text_size	=> text_size,
						processed	=> false),
					inserted => port_inserted,
					position => port_cursor
					);

				-- if port could not be inserted -> abort
				if not port_inserted then
					log (ERROR, "multiple usage of port " & f (element (line_cursor), 2) & " !");
					raise constraint_error;
				end if;
				
				log_indentation_down;
				next (line_cursor);
			end loop;

		else -- sheet has no ports -> warning
			log (WARNING, "hierarchic sheet " & to_string (submodule => sheet_name.name) & " has no ports !");
		end if;

		-- insert the hierarchical sheet in module (see type_module)
		add_hierarchic_sheet (sheet_name, sheet);

		log_indentation_down;
		
		exception
			when event:
				others =>
					log_indentation_reset;
					--log (message_error , console => true);
					log (text => ada.exceptions.exception_message (event));
					raise;

	end make_gui_sheet;


	function net_segment_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a net segment header like "Wire Wire Line"
		result : boolean := false;
	begin
		-- CS test field count
		if f (line,1) = schematic_keyword_wire then
			if f (line,2) = schematic_keyword_wire then
				if f (line,3) = schematic_keyword_line then
					result := true;
				end if;
			end if;
		end if;
		return result;
	end net_segment_header;
	
	procedure make_net_segment (
		lines			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is
	-- Builds a net segment and appends it to the collection of wild segments.

		-- After the segment heaser "Wire Wire Line" the next line like
		-- "2250 3100 2400 3100" is read here. It contains start and end points 
		-- of a net segment.
	
		segment : type_wild_net_segment; -- the segment being built
	begin
		--log (text => "making net segment ...", level => log_threshold);
		--log_indentation_up;

		line_cursor := pac_lines_of_file.first (lines);
		
		-- Build a temporarily net segment with fully specified coordinates:
		set_path (segment.coordinates_start, path_to_sheet);
		set_path (segment.coordinates_end, path_to_sheet);
		
		-- The sheet number.
		set_sheet (segment.coordinates_start, sheet_number);
		set_sheet (segment.coordinates_end, sheet_number);

		-- the x/y position
		set (X, mil_to_distance (f (element (line_cursor), 1)), segment.coordinates_start);
		set (Y, mil_to_distance (f (element (line_cursor), 2)), segment.coordinates_start);
		set (X, mil_to_distance (f (element (line_cursor), 3)), segment.coordinates_end);
		set (Y, mil_to_distance (f (element (line_cursor), 4)), segment.coordinates_end);

		-- Ignore net segments with zero length (CS: for some reason they may exist. could be a kicad bug)
		-- If a net segment has zero length, issue a warning.
		if length (segment) > zero then 

			-- The net segments are to be collected in a wild list of segments for later sorting.
			log (text => "net segment" & to_string (segment => segment, scope => xy), level => log_threshold);
			
			type_wild_segments.append (wild_segments, segment);
		else -- segment has zero length
			log (WARNING, affected_line (line) & "Net segment with zero length found -> ignored !");
		end if; -- length

		--log_indentation_down;
	end make_net_segment;

	function junction_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a net junction "Connection ~ 4650 4600"
		result : boolean := false;
	begin
		if et_string_processing.field_count (line) = 4 then
			if f (line,1) = schematic_keyword_connection then
				if f (line,2) = schematic_tilde then
					result := true;
				end if;
			end if;
		end if;
		return result;
	end junction_header;

	procedure make_junction (
		line			: in type_fields_of_line;
		log_threshold	: in type_log_level) is
	-- Builds a net junction and stores it both in the 
	-- junction list of the module (for statistics, ERC, ...) 
	-- AND in the wild list junctions.
	-- The wild list is needed when the anonymous strands of
	-- the sheet are built (see procedure build_anonymous_strands).
	-- The wild list contains the junction of the current sheet exclusively.
		junction : type_net_junction;  -- the junction being built

		procedure append_junction (
		-- add junction to module.junctions
			module_name : in type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) is
		begin
			type_junctions.append (
				container	=> module.junctions,
				new_item	=> junction);
		end append_junction;
		
	begin -- make_junction
		--log (text => "making net junction ...", level => log_threshold);
		--log_indentation_up;
		
		set_path (junction.coordinates, path_to_sheet);
		set_sheet (junction.coordinates, sheet_number);
		
		--set_x (junction.coordinates, mil_to_distance (f (line,3)));
		set (X, mil_to_distance (f (line,3)), junction.coordinates);
		
		--set_y (junction.coordinates, mil_to_distance (f (line,4)));
		set (Y, mil_to_distance (f (line,4)), junction.coordinates);

		-- for the log
		log (text => "net junction" & to_string (junction => junction, scope => xy), level => log_threshold);

		-- add to wild list of junctions
		type_junctions.append (wild_junctions, junction);

		-- add to module.junctions
		type_modules.update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> append_junction'access);

		--log_indentation_down;
	end make_junction;

	function simple_label_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a header of a simple label like 
	-- "Text Label 2350 3250 0 60 ~ 0"
		result : boolean := false;
	begin
		if et_string_processing.field_count (line) = 8 then
			if 	f (line,1) = schematic_keyword_text and 
				f (line,2) = schematic_keyword_label_simple then
					result := true;
			end if;
		end if;
		return result;
	end simple_label_header;

	procedure make_simple_label (
		lines 			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is
	-- Builds a simple net label and appends it to the collection of wild simple labels.

		-- The label header "Text Label 2350 3250 0 60 ~ 0" and the next line like
		-- "net_name_abc" is read here. It contains the supposed net name.

		use et_conventions;
		
		label : type_net_label_simple; -- the label being built
	begin
		--log (text => "simple label", level => log_threshold + 1);
		--log_indentation_up;
		
		line_cursor := pac_lines_of_file.first (lines);

		-- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
		set (X, mil_to_distance (f (element (line_cursor), 3)), label.coordinates);
		set (Y, mil_to_distance (f (element (line_cursor), 4)), label.coordinates);
		
		label.rotation := to_relative_rotation (f (element (line_cursor), 5));
		label.size := mil_to_distance (f (element (line_cursor), 6));
		--label.style := to_text_style (style_in => f (element (line_cursor), 7), text => true);
		label.width := mil_to_distance (f (element (line_cursor), 8));

		next (line_cursor);

		-- Make sure the label text (later this will be a net name) is not longer
		-- than allowed.
		check_net_name_length (f (element (line_cursor), 1));
		
		-- get label text and put it to temporarily simple label
		label.text := to_net_name (f (element (line_cursor), 1));

		-- Make sure there are no forbidden characters in the net name.
		check_net_name_characters (label.text);
		
		-- for the log
		--log (text => "simple label" & to_string (label => type_net_label (label), scope => xy), level => log_threshold);
		log (text => "simple label" & to_string (label => type_net_label (label)), level => log_threshold);

		check_schematic_text_size (category => net_label, size => label.size);
		-- CS: check label style
		-- CS: check label line width
		
		-- The simple labels are to be collected in a wild list of simple labels.
		type_simple_labels.append (wild_simple_labels, label);

		--log_indentation_down;
	end make_simple_label;

	function tag_label_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a header of a global or hierarchic label like 
	-- "Text HLabel 2700 2000 0 60 Input ~ 0" or
	-- "Text GLabel 4700 3200 1 60 UnSpc ~ 0"
		result : boolean := false;
	begin
		if et_string_processing.field_count (line) = 9 then
			if f (line,1) = schematic_keyword_text and 
				(f (line,2) = schematic_keyword_label_hierarchic or
				f (line,2) = schematic_keyword_label_global) then
					result := true;
			end if;
		end if;
		return result;
	end tag_label_header;

	procedure make_tag_label (
		lines 			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is
	-- Builds a global or hierachical label and appends it to the collection of wild tag labels.

		-- The label header "Text GLabel 4700 3200 1 60 UnSpc ~ 0" and the next line like
		-- "net_name_abc" is read here. It contains the supposed net name.

		use et_conventions;
		
		label : type_net_label_tag; -- the label being built
	begin
		--log (text => "making tag label ...", level => log_threshold);
		--log_indentation_up;

		line_cursor := pac_lines_of_file.first (lines);

		-- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
		-- The keyword in field 2 tells whether we have a hierarchic or global label:
		if f (element (line_cursor), 2) = schematic_keyword_label_hierarchic then
			label.hierarchic := true;
			label.global := false;
		else
			label.hierarchic := false;
			label.global := true;
		end if;

		set (X, mil_to_distance (f (element (line_cursor), 3)), label.coordinates);
		set (Y, mil_to_distance (f (element (line_cursor), 4)), label.coordinates);

		label.rotation := to_relative_rotation (f (element (line_cursor), 5));
		label.direction := to_direction (f (element (line_cursor), 7));

		-- build text attributes from size, font and line width
		label.size := mil_to_distance (f (element (line_cursor), 6));
		--label.style := to_text_style (style_in => f (element (line_cursor), 8), text => true);
		label.width := mil_to_distance (f (element (line_cursor), 9));

		next (line_cursor);

		-- Make sure the label text (later this will be a net name) is not longer
		-- than allowed.
		check_net_name_length (f (element (line_cursor), 1));
		
		-- get label text
		label.text := to_net_name (f (element (line_cursor), 1));
		
		-- Make sure there are no forbidden characters in the net name.
		check_net_name_characters (label.text);

		-- for the log
		--log (text => "tag label" & to_string (label => type_net_label (label), scope => xy), level => log_threshold);
		log (text => "tag label" & to_string (label => type_net_label (label)), level => log_threshold);

		check_schematic_text_size (category => net_label, size => label.size);
		-- CS: check style and line width
		
		-- The tag labels are to be collected in a wild list of tag labels for later sorting.
		type_tag_labels.append (wild_tag_labels, label);

		--log_indentation_down;
	end make_tag_label;

	function text_note_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a header of a text note like
	-- Text Notes 7100 6700 0 67 Italic 13
	-- ET Test Circuit
		result : boolean := false;
	begin
		if et_string_processing.field_count (line) = 8 then
			if f (line,1) = schematic_keyword_text and 
				f (line,2) = schematic_keyword_note then
					result := true;
			end if;
		end if;
		return result;
	end text_note_header;

	procedure make_text_note (
		lines			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is
	-- Builds a text note and appends it to the collection of text notes.

		-- The label header "Text Notes 3400 2800 0 60 Italic 12" and the next line like
		-- "ERC32 Test Board" is read here. It contains the actual text.
		
		note : type_text; -- the text note being built
		rotation : et_coordinates.type_rotation_relative;

		procedure warn is begin 
			log (WARNING, " text note at " 
				& et_kicad_coordinates.to_string (position => note.position, scope => SHEET) 
				& " might be misplaced !");
		end;
		
	begin -- make_text_note
		--log (text => "making text note ...", level => log_threshold);
		--log_indentation_up;
		
		line_cursor := pac_lines_of_file.first (lines);

		-- set coordinates
		set_path (note.position, path_to_sheet);
		set_sheet (note.position, sheet_number);

		set (X, mil_to_distance (f (element (line_cursor), 3)), note.position);
		set (Y, mil_to_distance (f (element (line_cursor), 4)), note.position);
		
		rotation := to_relative_rotation (f (element (line_cursor), 5));

		-- Notes might be upside down or readable from the left. So we must fit the rotation
		-- into a range between 0 and 90 degree:
		if rotation < 0.0 then
			note.rotation := 90.0;
			warn;
		--elsif rotation > 90.0 and rotation < 270.0 then
		elsif rotation > 90.0 then
			note.rotation := 0.0;
			warn;					
		--elsif rotation > 270.0 then
		--	note.rotation := 90.0;
		--	warn;
		end if;

		-- set text size and check for excessive size
		note.size := et_kicad_libraries.pac_text.to_text_size (mil_to_distance (f (element (line_cursor), 6)));
		
		--note.style := to_text_style (style_in => f (element (line_cursor), 7), text => true);

-- 				-- If the line width is too small, assume default and issue warning:
-- 				if mil_to_distance (f (element (line_cursor), 8)) < pac_text.type_text_line_width'first then
-- 					log (WARNING, "Line width too small. Defaulting to minimal width !");
-- 					note.line_width := pac_text.type_text_line_width'first;
-- 				else
-- 					note.line_width := mil_to_distance (f (element (line_cursor), 8));
-- 				end if;

		next (line_cursor);

		-- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
		-- CS: store lines in a list of lines instead ?
		-- CS: Currently we store the line as it is in tmp_note.text
		note.content := et_text.to_content (to_string (line));

		write_note_properties (note, log_threshold);
		
		-- the notes are to be collected in the list of notes
		add_note (note);

		--log_indentation_down;
	end make_text_note;

	function component_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a header of a component.
	-- The header is "$Comp"	
	begin
		if et_string_processing.field_count (line) = 1 then
			if f (line,1) = schematic_component_header then
				return true;
			else 
				return false;
			end if;
		else
			return false;
		end if;
	end component_header;

	function component_footer (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a footer of a component.
	-- The footer is "$EndComp"
	begin
		if et_string_processing.field_count (line) = 1 then
			if f (line,1) = schematic_component_footer then
				return true;
			else 
				return false;
			end if;
		else
			return false;
		end if;
	end component_footer;

	
	procedure make_component (
		lines 			: in pac_lines_of_file.list;
		log_threshold	: in type_log_level) is
	-- Builds a unit or a component and inserts it in the component list of 
	-- current module. The information required to make a component is provided
	-- in parameter "lines".

	-- Some entries of the component section are relevant for the whole component.
	-- Some entries are unit specific.
	-- The component section looks like this example:
	
	-- V4: L 74LS00 U1				-- component specific
	-- V5: L bel_logic:74LS00 U1	-- component specific
	-- U 4 1 5965E676	-- unit 1 of 4, link to package in board file
	-- P 4100 4000		-- unit position x/y
	-- AR Path="/59F17F77/5A991798" Ref="LED1"  Part="1" -- alternative reference
	-- AR Path="/5B7E59F3/5A991798" Ref="LED50"  Part="1" 
	-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
	-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
	-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
	-- F 3 "" H 4100 4000 50  0001 C CNN
	-- 	4    4100 4000		-- same as x/y pos

	--  1    0    0  -1  -- orientation 0,   mirror normal
	--  0   -1   -1   0  -- orientation 90,  mirror normal
	-- -1    0    0   1  -- orientation 180, mirror normal 
	-- 	0    1    1   0  -- orientation -90, mirror normal  

	-- 	1    0    0   1  -- orientation 0,   mirror --
	--  0   -1    1   0  -- orientation 90,  mirror -- 
	-- -1    0    0  -1  -- orientation 180, mirror -- 
	--  0    1   -1   0  -- orientation -90, mirror -- 

	-- -1    0    0  -1  -- orientation 0,   mirror |
	--  0    1   -1   0  -- orientation 90,  mirror |
	--  1    0    0   1  -- orientation 180, mirror |
	--  1    0    0   1  -- orientation -90, mirror |

		use et_schematic;

		reference					: type_device_name;	-- like IC5	
		appearance					: et_symbols.type_appearance := et_symbols.VIRTUAL; -- CS: why this default ?
		generic_name_in_lbr			: type_component_generic_name.bounded_string; -- like TRANSISTOR_PNP

		-- V5:
		component_library_name		: type_library_name.bounded_string; -- the name of the component library like bel_logic
		
		alternative_references		: type_alternative_references.list;
		unit_name					: pac_unit_name.bounded_string; -- A, B, PWR, CT, IO-BANK1 ...
		unit_position				: et_kicad_coordinates.type_position;
		orientation					: et_coordinates.type_rotation;
		mirror						: type_mirror;
		timestamp					: type_timestamp; -- 59F202F2
		alternative_representation	: type_de_morgan_representation;
	
		-- These are the "field found flags". They signal if a particular text field has been found.
		-- They are evaluated once the given lines are read completely.
		field_reference_found		: boolean := false;
		field_value_found			: boolean := false;
		field_package_found			: boolean := false;
		field_datasheet_found		: boolean := false;

		-- These are the actual fields that describe the component more detailled.
		-- They are contextual validated once the given lines are read completely.
		field_reference		: type_text_placeholder (meaning => NAME); -- like IC5 (redundant information with reference, see above)
		field_value			: type_text_placeholder (meaning => VALUE);	-- like 74LS00
		field_package		: type_text_placeholder (meaning => PACKGE); -- like "bel_primiteves:S_SOT23"
		field_datasheet		: type_text_placeholder (meaning => DATASHEET); -- might be useful for some special components
	
		function to_field return type_text_placeholder is
		-- Converts a field like "F 1 "green" H 2700 2750 50  0000 C CNN" to a type_text_placeholder
			text_position : type_point;
			size : et_kicad_libraries.pac_text.type_text_size;
			use et_text;
		begin
			-- test if the field content is longer than allowed:
			check_text_content_length (f (element (line_cursor), 3));
			
			set (X, mil_to_distance (f (element (line_cursor), 5)), text_position);
			set (Y, mil_to_distance (f (element (line_cursor), 6)), text_position);

			-- Kicad provides the absolute position of a text placeholder.
			-- But ET requires the position relative to the unit:
			text_position := type_point (distance_relative (text_position, type_point (unit_position)));
			
			size := mil_to_distance (f (element (line_cursor), 7));

			return (
				-- read text field meaning
				meaning 	=> to_text_meaning (line => element (line_cursor), schematic => true),

				-- read content like "N701" or "NetChanger" from field position 3
				content		=> to_content (f (element (line_cursor), 3)),

				-- read rotation like "H"
				rotation	=> to_field_orientation (f (element (line_cursor), 4)),

				-- read coordinates
				position	=> text_position,
								
				size		=> size,
				--style		=> to_text_style (style_in => f (element (line_cursor), 10), text => false),
				--line_width	=> text_line_width_default,

				-- build text visibility
				--visible		=> to_field_visible (
				--					vis_in		=> f (element (line_cursor), 8),
				--					schematic	=> true),

				-- build text alignment
				alignment	=> (
					horizontal	=> to_alignment_horizontal (f (element (line_cursor), 9)),
					vertical	=> to_alignment_vertical   (f (element (line_cursor), 10)))
				);
		end to_field;

		procedure check_text_fields (log_threshold : in type_log_level) is 
		-- Tests if any "field found" flag is still cleared and raises an alarm in that case.
		-- Perfoms a CONTEXTUAL VALIDATION of the text fields before they are used to 
		-- assemble and insert the component into the component list of the module.

			use et_conventions;
		
			procedure missing_field (m : in type_placeholder_meaning) is begin
				log (ERROR,
						"component " & to_string (reference) 
						& latin_1.space
						& to_string (position => unit_position)
						& latin_1.lf
						& "text field " & to_string (m) & " missing !",
					console => true);
				
				raise constraint_error;
			end missing_field;

			procedure process_alternative_references is
			-- Looks up alternative_references. The are provided in the schematic file in lines like:
			-- AR Path="/5B7CFC57/5A991D18" Ref="RPH19"  Part="1" 
			-- AR Path="/59F17FDE/5A991D18" Ref="RPH1"  Part="1" 
			-- The line with prenultimate timestamp (59F17FDE) that matches the current_schematic.timestamp
			-- dictates the new component reference (RPH1).
				use type_alternative_references;
				alt_ref_cursor : type_alternative_references.cursor := alternative_references.first;
				suitable_reference_found : boolean := false;

				procedure query_path (alt_ref : in type_alternative_reference) is
				-- queries paths like /59F17FDE/5A991D18 and compares the prenultimate timestamp
				-- with the current_schematic.timestamp. Sets the suitable_reference_found flag on match.
				-- Overwrites preliminary reference and content of field_reference.
					use type_alternative_reference_path;
					timestamp_cursor : type_alternative_reference_path.cursor := alt_ref.path.last;
				begin
					timestamp_cursor := previous (timestamp_cursor);
					if element (timestamp_cursor) = current_schematic.timestamp then
						
						reference := alt_ref.reference;
						field_reference.content := et_text.to_content (to_string (alt_ref.reference));
						suitable_reference_found := true;

						log (text => "update due to hierachic structure: " &
								to_string (reference), 
								level => make_component.log_threshold);
					end if;
				end query_path;
					
			begin -- process_alternative_references
				-- loop in list of alternative references and exit once a suitable one was found.
				while alt_ref_cursor /= type_alternative_references.no_element loop

					type_alternative_references.query_element (
						position	=> alt_ref_cursor,
						process		=> query_path'access);

					if suitable_reference_found then exit; end if;
					
					next (alt_ref_cursor);
				end loop;
			end process_alternative_references;
			
		begin -- check_text_fields
			log_indentation_up;

-- 					-- write precheck preamble
			log (text => "prechecking fields ...", level => log_threshold);
			log_indentation_up;
			
			-- reference
			-- NOTE: the reference prefix has been checked already in main of procedure make_component
			log (text => "reference", level => log_threshold + 1);
			if not field_reference_found then
				missing_field (NAME);
				-- CS: use missing_field (text_reference.meaning); -- apply this to other calls of missing_field too
			else
				-- If alternative references have been found, they must be looked up according to the
				-- timestamp of the current schematic (selector timestamp).
				-- Otherwise the reference must be verified aginst the content of the reference field.
				-- Reason: KiCad stores redundant information on the component reference as in this example;
					-- $Comp
					-- L 74LS00 IC1 <- reference
					-- U 1 1 59969711
					-- P 4100 4000
					-- F 0 "IC1" H 4100 4050 50  0000 C BIB <- text_reference

				if type_alternative_references.is_empty (alternative_references) then -- no alternative references
					log (text => "reference " & to_string (reference), level => log_threshold + 1);
					
					if to_string (reference) /= content (field_reference) then
						log (ERROR, "reference mismatch ! Header reads " 
							& to_string (reference) & " but field contains " 
							& content (field_reference),
							console => true);
						raise constraint_error;
					end if;

				else -- alternative references found
					process_alternative_references;
				end if;

				check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_reference.size);
			end if;

			-- value
			log (text => "value", level => log_threshold + 1);
			if not field_value_found then
				missing_field (VALUE);
			else
				check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_value.size);
			end if;

			-- If we are checking fields of a real component there are more 
			-- fields to be checked. If it is about a virtual component, those 
			-- fields are ignored and thus NOT checked:
			case appearance is
				when et_symbols.PCB =>
						
					-- package
					log (text => "package/footprint", level => log_threshold + 1);
					if not field_package_found then
						missing_field (PACKGE);
					else
						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_package.size);
						-- CS: check content of field_package
						-- use library_name (content (field_package))
						-- check/validate library name (length, characters, ...)
						-- make sure the library exists. mind search order of footprint libraries

						-- check/validate package name (length, characters, ...)
						et_packages.check_package_name_length (et_packages.to_string (package_name (content (field_package))));
						et_packages.check_package_name_characters (package_name (content (field_package)));
					end if;

					-- datasheet
					log (text => "datasheet", level => log_threshold + 1);
					if not field_datasheet_found then
						missing_field (DATASHEET);
					else
						check_schematic_text_size (category => COMPONENT_ATTRIBUTE, size => field_datasheet.size);
						-- CS: check content of field_datasheet
					end if;
					
				when others => null; -- CS ?
			end case;

			log_indentation_down;
			log_indentation_down;
			
			exception
				when event:
					others =>
						log (ERROR, 
							"invalid field in component " & to_string (reference)
							& to_string (position => unit_position),
							console => true);
						log (text => ada.exceptions.exception_message (event), console => true);
						-- CS: evaluate prog position and provided more detailled output
						raise;

		end check_text_fields;

		function generic_name_to_library (
		-- Returns the full name of the library where given generic component is contained.
		-- The given reference serves to provide a helpful error message on the affected 
		-- component in the schematic.
			component 		: in type_component_generic_name.bounded_string; -- the generic name like "RESISTOR"
			reference 		: in type_device_name; -- the reference in the schematic like "R4"
			log_threshold	: in et_string_processing.type_log_level)
			return type_device_library_name.bounded_string is -- the full library name like "../libraries/resistors.lib"

			use type_device_libraries;
			use type_device_library_name;
		
			component_found : boolean := false; -- goes true once the given component was found in any library
			
			lib_cursor : type_device_libraries.cursor := tmp_component_libraries.first; -- points to the library being searched in
			library : type_device_library_name.bounded_string; -- the full library name to be returned

			procedure query_components (
			-- Queries the components in the current library. Exits prematurely once the 
			-- given generic component was found.
				lib_name 	: in type_device_library_name.bounded_string;
				components 	: in type_components_library.map) is
				use type_components_library;
				component_cursor : type_components_library.cursor := components.first;
				--use type_component_generic_name;
			begin
				log_indentation_up;
				while component_cursor /= type_components_library.no_element loop
					
					log (text => to_string (key (component_cursor)), level => log_threshold + 2);

					-- Sometimes generic names in the library start with a tilde. it must
					-- be removed before testing the name.
					if type_component_generic_name."=" (strip_tilde (key (component_cursor)), component) then
						component_found := true;
						exit;
					end if;
					next (component_cursor);

				end loop;
				log_indentation_down;
			end query_components;
			
		begin -- generic_name_to_library
			log_indentation_up;
			log (text => "locating library containing generic component " & to_string (component) & " ...", level => log_threshold);
			
			-- loop in libraries and exit prematurely once a library with the given component was found
			while lib_cursor /= type_device_libraries.no_element loop
				log_indentation_up;
				log (text => "probing " 
						& et_devices.to_string (key (lib_cursor)) 
						& " ...", level => log_threshold + 1);

				query_element (
					position	=> lib_cursor,
					process		=> query_components'access);

				log_indentation_down;
				
				-- Exit BEFORE advancing the lib_cursor because lib_cursor position must be kept fore
				-- return value. See below. component_found MUST NOT be parameter of this loop.
				if component_found then
					exit;
				end if;
				
				next (lib_cursor);

			end loop;
			log_indentation_down;
			
			-- After a successful search return the name of the library where lib_cursor is pointing to.
			-- Otherwise send error messagen and abort.
			if component_found then
				return key (lib_cursor);
			else
				log (ERROR, "for component "  
					& to_string (reference)
					& " no generic model in any library found !",
					console => true);
				raise constraint_error;
			end if;
			
		end generic_name_to_library;

		function full_name_of_component_library (
		-- The given reference serves to provide a helpful error message on the affected 
		-- component in the schematic.
			component 		: in type_component_generic_name.bounded_string; -- the generic name like "RESISTOR"
			reference 		: in type_device_name; -- the reference in the schematic like "R4"
			log_threshold 	: in type_log_level) 
			return type_device_library_name.bounded_string is

			use type_lib_table;
			sym_lib_cursor : type_lib_table.cursor := sym_lib_tables.first;

			use type_device_libraries;
			lib_cursor : type_device_libraries.cursor;
			
			use type_library_name;
			
			full_name : type_device_library_name.bounded_string;
			component_found : boolean := false;

			procedure search_component (
			-- Seaches a component library for the given generic component.
				lib_name	: in type_device_library_name.bounded_string;
				lib			: in type_components_library.map) is
				use type_components_library;
			begin
				if contains (lib, component) then
					component_found := true;
				end if;
			end search_component;
			
		begin -- full_name_of_component_library
			log_indentation_up;
			log (text => "locating library '" & et_kicad_general.to_string (component_library_name) & "' containing generic component '" 
					& to_string (component) & "' ...", level => log_threshold);

			-- Search in the sym-lib-table for the first an entry having the component_library_name (uri)
			while sym_lib_cursor /= type_lib_table.no_element loop
				if element (sym_lib_cursor).lib_name = component_library_name then
					full_name := element (sym_lib_cursor).lib_uri;

					-- locate component library by full_name
					lib_cursor := type_device_libraries.find (tmp_component_libraries, full_name);
					
					-- Test if library contains the given generic component.
					type_device_libraries.query_element (
						position	=> lib_cursor,
						process		=> search_component'access);
					
				end if;

				-- Cancel search once the given generic component has been found. Otherwise
				-- proceed with next same named library in sym-lib-table.
				if component_found then exit; end if;
				
				next (sym_lib_cursor);
			end loop;

			log_indentation_down;
			
			-- After a successful search return the name of the library where lib_cursor is pointing to.
			-- Otherwise send error messagen and abort.
			if component_found then
				return full_name;
			else
				log (ERROR, "for component "  
					& to_string (reference)
					& " no generic model in any library named '" & et_kicad_general.to_string (component_library_name) 
					& "' found !",
					console => true);
				raise constraint_error;
			end if;

		end full_name_of_component_library;

		function remove_leading_hash (reference : in type_device_name) return
		-- Removes from a reference like #PWR04 the leading hash character.
		-- CS: This function should be applied on virtual components (such as power flags or power symbols) only.
		-- The assumption is that their prefix always starts with a hash character.
			type_device_name is
			use pac_device_prefix;
			reference_out : type_device_name := reference; -- to be returned -- like PWR04
		begin
			--log (text => "renaming " & to_string (reference_out));
			--log (text => "length" & positive'image (length (reference_out.prefix)));
			reference_out.prefix := to_bounded_string (slice (reference_out.prefix, 2, length (reference_out.prefix)));
			--log (text => "prefix new '" & pac_device_prefix.to_string (reference_out.prefix) & "'");
			--log (text => " to " & to_string (reference_out));
			return reference_out;
		end remove_leading_hash;
		
		procedure insert_component is
		-- Inserts the component in the component list of the module (indicated by module_cursor).
		-- Components may occur multiple times, which implies they are
		-- split into units (EAGLE refers to them as "gates").
		-- Only the first occurence of the component leads to appending it to the component list of the module.
		
		-- The component to be inserted gets assembled from the temporarily variables assigned until now.
		-- Tests if a footprint has been associated with the component.

			full_component_library_name : type_device_library_name.bounded_string;

			use et_symbols;
			use et_import;
			
		begin -- insert_component

			case cad_format is
				when KICAD_V4 =>
					-- KiCad V4 does not provide an exact name of the library where the generic component
					-- model can be found. It only provides the generic name of the model.
					-- The library is determined by the order of the library names in the 
					-- project file. It is the first library in this list that contains the model.
					-- The function generic_name_to_library does the job and sets the full_component_library_name
					-- here right away:
					full_component_library_name := generic_name_to_library (
							component		=> generic_name_in_lbr, -- 7400
							reference		=> reference,			-- IC1
							log_threshold	=> log_threshold + 3);

				when KICAD_V5 =>
					-- KiCad V5 provides a simple name for the component library along with the generic 
					-- component name. From the library name we must deduce the full library name.
					full_component_library_name := full_name_of_component_library (
							component		=> generic_name_in_lbr,	-- 7400
							reference		=> reference,			-- IC1
							log_threshold	=> log_threshold + 3);

				when others => raise constraint_error;
			end case;
			
			log_indentation_up;
			
			-- The component is inserted into the components list of the module according to its appearance.
			-- If the component has already been inserted, it will not be inserted again.
			-- CS: Even if the component is not inserted again, all the operations that form its elements
			-- like power_flag, library_name, ... are executed which is a waste of computing time.
			
			case appearance is
				
				when VIRTUAL => -- we have a line like "L P3V3 #PWR07"
			
					add_component (
						reference	=> remove_leading_hash (reference), -- #PWR03 becomes PWR03
						component	=> (
							appearance		=> et_symbols.VIRTUAL,

							-- Whether the component is a "power flag" can be reasoned from its reference:
							power_flag		=> to_power_flag (reference),

							library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
							generic_name	=> generic_name_in_lbr,
							alt_references	=> alternative_references,
							
							value 			=> to_value_with_check (
												value => content (field_value),
												error_on_invalid_character => false),
								-- For the operators convenice no error is raised if invalid
								-- character found. This was the design gets imported but with
								-- (lots of) warnings.
							
							-- At this stage we do not know if and how many units there are. So the unit list is empty.
							units 			=> type_units_schematic.empty_map),
						log_threshold => log_threshold + 2);

				when et_symbols.PCB => -- we have a line like "L 74LS00 U1"

					add_component ( 
						reference => reference,
						component => (
							appearance		=> et_symbols.PCB,

							library_name	=> full_component_library_name, -- ../lbr/bel_logic.lib
							generic_name	=> generic_name_in_lbr,
							alt_references	=> alternative_references,
							
							value 			=> to_value_with_check (
												value => content (field_value),
												error_on_invalid_character => false),
								-- For the operators convenice no error is raised if invalid
								-- character found. This was the design gets imported but with
								-- (lots of) warnings.

							-- properties of a real component (appears in schematic and layout);
							datasheet		=> type_component_datasheet.to_bounded_string (content (field_datasheet)),

							-- the package variant is determined by the package library and package name:
							variant			=> to_package_variant (
													component_library	=> full_component_library_name, -- ../lbr/bel_logic.lib
													generic_name		=> generic_name_in_lbr, -- 7400
													package_library		=> library_name (content (field_package)), -- bel_ic
													package_name		=> package_name (content (field_package)), -- S_SO14
													log_threshold		=> log_threshold + 2),

							-- This is layout related and will be filled on layout import later (much later):
							position			=> et_pcb_coordinates.package_position_default, -- the position of the package in the layout
							text_placeholders	=> (others => <>),  -- placeholders for reference, value, purpose in the layout
							
							-- At this stage we do not know if and how many units there are. So the unit list is empty for the moment.
							units => type_units_schematic.empty_map),

						log_threshold => log_threshold + 2);

						-- Test if footprint has been associated with the component.
						if content (field_package)'size = 0 then
							log (ERROR, "component " & to_string (reference) 
									& " footprint not specified !",
								console => true);
							raise constraint_error;
						end if;

			end case;

			log_indentation_down;
			
			exception
				when constraint_error =>
					log (ERROR, "component " & to_string (reference)
							& " " & to_string (position => unit_position),
						console => true);
					raise constraint_error;
			
		end insert_component;

		procedure insert_unit is 
		-- Inserts a unit into the unit list of a component. The text fields around a unit are placeholders.
		-- The properties of the placeholder texts are loaded with the properties of the text fields of the units
		-- found in the schematic. The idea behind is to store just basic text properties (type_text_basic) 
		-- for the texts around the unit, but not its content. The content is stored with the component as a kind
		-- of meta-data. See procedure insert_component.
		-- Raises constraint error if unit already in unit list of component.
			use et_symbols;
		begin
			log_indentation_up;
			
			case appearance is

				when VIRTUAL =>

					add_unit (
						reference	=> remove_leading_hash (reference), -- #PWR03 becomes PWR03
						unit_name	=> unit_name, -- "I/O Bank 3" or "PWR" or "A" or "B" ...	
						unit 		=> (
							appearance		=> VIRTUAL,
							position		=> unit_position,
							rotation		=> orientation,
							mirror			=> mirror,
							timestamp		=> timestamp,
							alt_repres		=> alternative_representation,

							-- placeholders:
							reference		=> (
									meaning		=> NAME,
									position	=> field_reference.position,
									--style		=> field_reference.style,
									rotation	=> snap (field_reference.rotation),
									size		=> field_reference.size,
									alignment	=> field_reference.alignment),

							value			=> (
									meaning		=> VALUE,
									position	=> field_value.position,
									--style		=> field_value.style,
									rotation	=> snap (field_value.rotation),
									size		=> field_value.size,
									alignment	=> field_value.alignment)
									),
						
						log_threshold => log_threshold + 2);
										

				when et_symbols.PCB =>

					add_unit 
						(
						reference	=> reference,
						unit_name	=> unit_name, -- "I/O Bank 3" or "PWR" or "A" or "B" ...	
						unit 		=> 
							(
							appearance		=> et_symbols.PCB,
							position		=> unit_position,
							rotation		=> orientation,
							mirror			=> mirror,
							timestamp		=> timestamp,
							alt_repres		=> alternative_representation,

							-- The kicad placeholders are now converted to ET native placeholders:
						
							reference		=> (
									meaning		=> NAME,
									position	=> field_reference.position,
									--style		=> field_reference.style,
									rotation	=> snap (field_reference.rotation),
									size		=> field_reference.size,
									alignment	=> field_reference.alignment),

							value			=> (
									meaning		=> VALUE,
									position	=> field_value.position,
									--style		=> field_value.style,
									rotation	=> snap (field_value.rotation),
									size		=> field_value.size,
									alignment	=> field_value.alignment)
							),
						log_threshold => log_threshold + 2
						);

			end case;

			log_indentation_down;
		end insert_unit;

		procedure verify_unit_name_and_position (line : in type_fields_of_line) is
		-- Checks if the x/y position of the unit matches that provided in given line.
		-- It is about the strange repetition of the unit name and its x/y coordinates in a line like
		-- "2    6000 4000"
		begin -- verify_unit_name_and_position
			
			if et_devices.to_string (unit_name) /= f (line,1) then
				log (ERROR, "invalid unit name '" & f (line,1) & "'", console => true);
				raise constraint_error;
			end if;
			
			if x (unit_position) /= mil_to_distance (f (line,2)) then
-- 					log (text => "position invalid. expected '" & to_string (position.x) 
-- 						& "' found '" 
-- 						& field (line,2)
-- 						& "'");
				raise constraint_error; -- CS: write useful message
			end if;

			if y (unit_position) /= mil_to_distance (f (line,3)) then
				raise constraint_error; -- CS: write useful message
			end if;

		end verify_unit_name_and_position;

		procedure build_unit_orientation_and_mirror_style (line : in type_fields_of_line) is
		-- Builds from a line (see below) the component orientation and mirror style:

			-- Angles in Kicad are to be interpreted as: 
			-- positive angle -> counter clock wise
			-- negative angle -> clock wise

			-- The order of operations: FIRST rotate THEN mirror
			
			--  1    0    0  -1  -- orientation 0,   mirror normal
			--  0   -1   -1   0  -- orientation 90,  mirror normal
			-- -1    0    0   1  -- orientation 180, mirror normal 
			-- 	0    1    1   0  -- orientation -90, mirror normal  

			-- 	1    0    0   1  -- orientation 0,   mirror --
			--  0   -1    1   0  -- orientation 90,  mirror -- 
			-- -1    0    0  -1  -- orientation 180, mirror -- 
			--  0    1   -1   0  -- orientation -90, mirror -- 

			-- -1    0    0  -1  -- orientation 0,   mirror | 	-- not used
			--  0    1   -1   0  -- orientation 90,  mirror |	-- not used
			--  1    0    0   1  -- orientation 180, mirror |	-- not used
			--  1    0    0   1  -- orientation -90, mirror |	-- not used

			orient_1, orient_2 : type_schematic_unit_orientation;
			mirror_1, mirror_2 : type_schematic_unit_mirror_style;
		
		begin -- CS: provide useful log messages via exception handler

			-- compute unit orientation
			orient_1 := type_schematic_unit_orientation'value (f (line, 1));
			orient_2 := type_schematic_unit_orientation'value (f (line, 2));
			mirror_1 := type_schematic_unit_mirror_style'value (f (line, 3));
			mirror_2 := type_schematic_unit_mirror_style'value (f (line, 4));

			case orient_1 is
				when -1 =>
					if orient_2 = 0 then
						orientation := 180.0;

						-- compute unit mirror style
						if mirror_1 = 0 then
							case mirror_2 is
								when -1 =>
									mirror := X_AXIS;
								when  1 =>
									mirror := NO;
								when others =>
									-- invalid mirror style
									raise constraint_error;
							end case;
						else
							-- invalid mirror style
							raise constraint_error;
						end if;
						
					else
						-- invalid orientation
						raise constraint_error;
					end if;
						
				when  0 =>
					case orient_2 is
						when -1 => 
							orientation := 90.0;
							
							-- compute unit mirror style
							case mirror_1 is
								when -1 =>
									if mirror_2 = 0 then
										mirror := NO;
									else
										-- invalid mirror style
										raise constraint_error;
									end if;

								when  0 =>
									-- invalid mirror style
									raise constraint_error;

								when  1 =>
									if mirror_2 = 0 then
										mirror := X_AXIS;
									else
										-- invaid mirror style
										raise constraint_error;
									end if;
							end case;

						when  1 =>
							orientation := -90.0;

							-- compute unit mirror style
							case mirror_1 is
								when -1 =>
									if mirror_2 = 0 then
										mirror := X_AXIS;
									else
										-- invalid mirror style
										raise constraint_error;
									end if;

								when  0 =>
									-- invaid mirror style
									raise constraint_error;

								when  1 =>
									if mirror_2 = 0 then
										mirror := NO;
									else
										-- invalid mirror style
										raise constraint_error;
									end if;
							end case;

						when others => 
							-- invalid orientation
							raise constraint_error;
					end case;

				when  1 =>
					if orient_2 = 0 then
						orientation := 0.0;

						-- compute unit mirror style
						if mirror_1 = 0 then
							case mirror_2 is
								when -1 =>
									mirror := NO;
								when  1 =>
									mirror := X_AXIS;
								when others =>
									-- invalid mirror style
									raise constraint_error;
							end case;
						else
							-- invalid mirror style
							raise constraint_error;
						end if;
						
					else
						-- invalid orientation
						raise constraint_error;
					end if;
			end case;

		end build_unit_orientation_and_mirror_style;

		procedure add_alternative_reference (line : in type_fields_of_line) is
		-- Adds the alternative reference given in a line like 
		-- AR Path="/5B7E59F3/5B7E5817" Ref="#PWR03"  Part="1" 
		-- to the list alternative_references.
		
			path	: et_string_processing.type_fields_of_line; -- 59F17F77 5A991798
			ref		: type_device_name; -- #PWR03
			unit	: pac_unit_name.bounded_string; -- 1 -- CS is this really about unit names ?

			path_segment : type_timestamp;
			alt_ref_path : type_alternative_reference_path.list;
		begin
			log (text => "alternative reference " & et_string_processing.to_string (line), level => log_threshold + 3); -- Path="/59F17F77/5A991798
			--log (text => field (line, 2) (8 .. field (line, 2)'last), level => log_threshold + 1);
			
			-- extract the path segments from field 2: example: Path="/59F17F77/5A991798					
			path := et_string_processing.read_line (
				line			=> trim (f (line, 2) (8 .. f (line, 2)'last), both), -- 59F17F77/5A991798
				-- NOTE: the trailing double quote is already gone.
				
				comment_mark	=> "", -- no comment marks
				ifs				=> hierarchy_separator (1)); -- hierarchy_separator is a string

			--log (text => et_string_processing.to_string (path), level => log_threshold + 1);
			
			-- Transfer the path segments to alt_ref_path.
			-- "path" contains a list of strings.
			-- alt_ref_path is a list of timestamps
			for place in 1 .. et_string_processing.field_count (path) loop

				-- convert the segment from string to timestamp
				path_segment := type_timestamp (f (path, place));

				-- append the segment
				type_alternative_reference_path.append (	
					container	=> alt_ref_path,
					new_item	=> path_segment);
			end loop;

-- 					log (text => "new reference '" & field (line, 3) (6.. (field (line, 3)'last)) & "'", level => log_threshold + 1);  -- #PWR03
			
			-- extract the reference from field 3: example: Ref="#PWR03
			-- NOTE: the trailing double quote is already gone.
			ref := to_component_reference (
					text_in 		=> f (line, 3) (6.. (f (line, 3)'last)),  -- #PWR03
					leading_hash	=> true);

-- 					log (text => "test", level => log_threshold + 1);
-- 					log (text => "new reference " & et_libraries.to_string (ref), level => log_threshold + 1);  -- #PWR03
			
			-- extract the part name (CS unit name ?) from field 4: example Part="1
			-- NOTE: the trailing double quote is already gone.
			unit := to_unit_name (f (line, 4) (7 .. (f (line, 4)'last)));

			-- Now all components of the alternative reference are ready.
			-- Append the new alternative reference to list alternative_references:
			type_alternative_references.append (
				container	=> alternative_references,
				new_item	=> (
								path		=> alt_ref_path,
								reference	=> ref,
								part		=> unit
								));
			
		end add_alternative_reference;
		
		function generic_name (text : in string) return type_component_generic_name.bounded_string is
		-- Extracts from a given string like "bel_logic:7400" the generic component name "7400".
			ifs : constant string (1..1) := ":";

			-- The separator must NOT be at first position in text.
			-- CS: Text is limited to 200 characters which seems sufficient.
			subtype type_pos is positive range 2 .. 200;

			pos : type_pos := index (text, ifs); -- get position of ifs
		begin -- generic_name
			return type_component_generic_name.to_bounded_string (text (pos + 1 .. text'last)); -- 7400
		end generic_name;

		function extract_library_name (text : in string) return type_library_name.bounded_string is
		-- Extracts from a given string like "bel_logic:7400" the library name "bel_logic".
			ifs : constant string (1..1) := ":";

			-- The separator must NOT be at first position in text.
			-- CS: Text is limited to 200 characters which seems sufficient.
			subtype type_pos is positive range 2 .. 200;

			pos : type_pos := index (text, ifs); -- get position of ifs
		begin -- extract_library_name
			return type_library_name.to_bounded_string (text (text'first .. pos - 1)); -- bel_logic
		end extract_library_name;

		use et_symbols;
		
	begin -- make_component (schematic)
		log (text => "making component ...", level => log_threshold);
		log_indentation_up;

		-- loop in lines provided by "lines"
		line_cursor := pac_lines_of_file.first (lines);
		while line_cursor /= pac_lines_of_file.no_element loop

			log (text => "component line: " & to_string (element (line_cursor)), level => log_threshold + 6);

			-- V4: 
			--	- Read component generic name and annotation from a line like "L NetChanger N1".
			-- V5:
			--	- Read library name, component generic name and annotation from a line like "L bel_logic:7400 IC1". 
			
			-- From this entry we reason the component appearance. 
			-- The appearance is important for contextual validation of the fields.
			-- It is also required for validation of the reference (like R12 or C4).
			if f (element (line_cursor), 1) = schematic_component_identifier_name then -- "L"

				case et_import.cad_format is
					when et_import.KICAD_V4 =>
						generic_name_in_lbr := type_component_generic_name.to_bounded_string (
												f (element (line_cursor), 2)); -- "SN74LS00"

					when et_import.KICAD_V5 =>
						generic_name_in_lbr := generic_name (f (element (line_cursor), 2)); -- "bel_logic:SN74LS00"
						component_library_name := extract_library_name (f (element (line_cursor), 2)); -- "bel_logic:SN74LS00"

					when others => raise constraint_error;
				end case;
						
				log (text => "generic name " & to_string (generic_name_in_lbr), level => log_threshold + 1);
				
				check_generic_name_characters (
					name => generic_name_in_lbr, -- "SN74LS00"
					-- NOTE: We do not allow tilde characters here. they occur ONLY in the library:
					characters => component_generic_name_characters); 

				appearance := to_appearance (line => element (line_cursor), schematic => true);
				log (text => to_string (appearance, verbose => true), level => log_threshold + 3);

				-- Depending on the appearance of the component the reference is built and checked.
				-- IMPORTANT: The reference is preliminary. Due to possible hierarchic design, it
				-- might be overwritten once alternative references are found in this sheet.
				case appearance is
				
					when VIRTUAL => 
						-- We have a line like "L P3V3 #PWR07".
						-- Build a reference type from the given reference string.
						-- Afterward we validate the prefix of the reference. It must be
						-- a power symbol or a power flag (#PWR or #FLG).
						reference := to_component_reference (
							text_in			=> f (element (line_cursor), 3),
							leading_hash	=> true); 

						log (text => "reference " & to_string (reference) & " (preliminary)", level => log_threshold);
						validate_prefix (reference);

					when et_symbols.PCB =>
						-- we have a line like "L 74LS00 IC13"
						-- -- Build a reference type from the given reference string.
						-- Afterward we validate the prefix of the reference. 
						-- It is about a REAL component. Its prefix must be one 
						-- of those defined in the configuration file (see et_conventions).
						reference := to_component_reference ( -- character check included
							text_in			=> f (element (line_cursor), 3),
							leading_hash	=> false);

						log (text => "reference " & to_string (reference) & " (preliminary)", level => log_threshold);
						
				end case;
							
				-- CS: check proper annotation

			-- read line like "U 2 1 4543D4D3F" 
			-- U is the line indicator, 2 is the unit id, 1 is the demorgan flag.
			-- Last field is the link to the package in the board file.
			elsif f (element (line_cursor), 1) = schematic_component_identifier_unit then -- "U"

				-- KiCad uses positive numbers to identifiy units. But in general a unit name can
				-- be a string as well. Therefore we handle the unit id as string.
				unit_name := pac_unit_name.to_bounded_string ( -- CS: check_unit_name_characters
					f (element (line_cursor), 2)); -- the unit id

				-- Read DeMorgan flag:
				alternative_representation := to_alternative_representation (
					line => element (line_cursor), schematic => true);

				-- Read and check the link to the board file:
				timestamp := type_timestamp (f (element (line_cursor), 4));

			-- Read unit coordinates from a line like "P 3200 4500".
			elsif f (element (line_cursor), 1) = schematic_component_identifier_coord then -- "P"
			
				set (X, mil_to_distance (f (element (line_cursor), 2)), unit_position); -- "3200"
				set (Y, mil_to_distance (f (element (line_cursor), 3)), unit_position); -- "4500"

				-- The unit coordinates is more than just x/y. We also have path and sheet number:
				set_path (unit_position, path_to_sheet);
				set_sheet (unit_position, sheet_number);

			-- Read alternative reference like "AR Path="/59EF082F" Ref="N23"  Part="1"
			elsif f (element (line_cursor), 1) = schematic_component_identifier_path then -- "AR"
				add_alternative_reference (element (line_cursor));

			-- read unit fields 0..2 from lines like:
			-- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
			--			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
			--			"F 2 "bel_netchanger:0.2MM" H 2600 2100 60  0001 C CNN"

			-- Set "field found flags" accordingly.
			-- Do some basic checks on the fields.
			elsif f (element (line_cursor), 1) = component_field_identifier then -- "F"

				--log (text => "unit field A: " & to_string (line));
				
				case type_component_field_id'value (f (element (line_cursor), 2)) is
					
					when component_field_reference =>
						field_reference_found := true;
						field_reference := to_field;
						-- NOTE: This is a redundant field. Its content must match the reference (see above).
						-- This test is performed in procedure check_text_fields.
						
					when component_field_value =>
						field_value_found := true;
						field_value := to_field;

						declare
							value : pac_device_value.bounded_string;
						begin
							value := et_devices.to_value_with_check (
									value 						=> content (field_value),
									error_on_invalid_character	=> false);
							-- For the operators convenice no error is raised if invalid
							-- character found. This was the design gets imported but with
							-- (lots of) warnings.
						end;

						
					when component_field_package =>
						field_package_found := true;
						field_package := to_field;
						et_packages.check_package_name_length (content (field_package));
						et_packages.check_package_name_characters (
							packge		=> et_packages.pac_package_name.to_bounded_string (content (field_package)),
							characters	=> component_package_name_characters);
						
					when component_field_datasheet =>
						field_datasheet_found := true;
						field_datasheet := to_field;
						check_datasheet_length (content (field_datasheet));
						check_datasheet_characters (
							datasheet => type_component_datasheet.to_bounded_string (content (field_datasheet)));
						
					when others => null; -- ignore other fields
				end case;

				--log (text => "unit field B: " & to_string (line));
				
			else
				-- What is left is a strange repetition of the unit name and its x/y coordinates in a line like
				-- "2    6000 4000"
				-- followed by the unit mirror style and the unit orientation in a line like
				-- "1    0    0    -1"

				case field_count (element (line_cursor)) is
					when 3 => -- we have the unit name and its x/y position.
						-- We verify if unit name and position match the values read earlier:
						verify_unit_name_and_position (element (line_cursor));
					
					when 4 => -- we have the unit mirror style and orientation
						build_unit_orientation_and_mirror_style (element (line_cursor));
					
					when others => 
						raise constraint_error; -- CS: write useful message
				end case;

			end if;


			next (line_cursor);
		end loop;

		-- Check whether all required text fields have been found.
		-- Check content of text fields for syntax and plausibility.
		check_text_fields (log_threshold + 1);
		
		-- Insert component in component list of module. If a component is split
		-- in units, only the first occurence of it leads to inserting the component.
		-- Nevertheless there are some checks on the unit (see insert_component).
		insert_component;

		-- We update the component with the collected unit information.
		insert_unit;

		log_indentation_down;
		
		exception
			when event:
				others =>
					if line_cursor /= pac_lines_of_file.no_element then
						error_in_schematic_file (line);
					end if;
						
					--log (text => ada.exceptions.exception_message (event), console => true);
					raise;
		
	end make_component;

	function no_connection_header (line : in type_fields_of_line) return boolean is
	-- Returns true if given line is a no-connection-flag "NoConn ~ 5000 3900"
		result : boolean := false;
	begin
		-- CS test field count
		if f (line,1) = schematic_keyword_no_connection then
			if f (line,2) = schematic_tilde then
				result := true;
			end if;
		end if;
		return result;
	end no_connection_header;
	
	procedure make_no_connection (line : in type_fields_of_line) is
	-- Builds a no-connect flag and stores it a wild list of no-connection-flags
	-- A line that specifies such a flag loops like "NoConn ~ 5000 3900"
		no_connection_flag : type_no_connection_flag;

		use type_modules;
	
		procedure append_no_connect_flag (
			module_name	: in type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) is
			use type_no_connection_flags;
		begin
			append (
				container => module.no_connections,
				new_item => no_connection_flag);
		end append_no_connect_flag;				
	
	begin -- make_no_connection
		set_path (no_connection_flag.coordinates, path_to_sheet);
		set_sheet (no_connection_flag.coordinates, sheet_number);
		
		--set_x (no_connection_flag.coordinates, mil_to_distance (f (line,3)));
		set (X, mil_to_distance (f (line,3)), no_connection_flag.coordinates);
		--set_y (no_connection_flag.coordinates, mil_to_distance (f (line,4)));
		set (Y, mil_to_distance (f (line,4)), no_connection_flag.coordinates);
		
		-- for the log
		log (text => "no-connection-flag" & to_string (no_connection_flag => no_connection_flag, scope => xy),
				level => log_threshold + 1);

		-- append the no-connect-flag to the list of no_connections of the current module
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> append_no_connect_flag'access);
		
	end make_no_connection;
	
begin -- read
	
	log_indentation_reset;
	log_indentation_up;

	if exists (to_string (current_schematic.sheet.file)) then
		log (text => "reading schematic file " & to_string (current_schematic.sheet.file) &
			" sheet name " & to_string (current_schematic.sheet.name) &
			" with timestamp " & string (current_schematic.timestamp) & " ...",
				level => log_threshold,
				console => true);

		-- log module path as recorded by parent unit
		log_indentation_up;
		log (text => "path " & to_string (path_to_sheet), level => log_threshold);
		
		open (file => schematic_handle, mode => in_file, name => to_string (current_schematic.sheet.file));
		set_input (schematic_handle);

		-- read schematic file line per line
		while not end_of_file loop

			-- Store line in variable "line" (see et_string_processing.ads)
			line := et_string_processing.read_line (
						line 			=> get_line,
						number 			=> ada.text_io.line (current_input),
						comment_mark 	=> "", -- there are no comment marks in the schematic file
						delimiter_wrap 	=> true, -- there are fields wrapped in delimiters
						ifs 			=> latin_1.space); -- fields are separated by space
			-- CS: If read_line exits with an exception, the exception handler of read_schematic
			-- outputs the line BEFORE the faulty line. Thus misleading the operator.
			
			case field_count (line) is
				when 0 => null; -- we skip empty lines
				when others =>

					-- At a certain log level we report the whole line as it is:
					log (text => "line ->" & to_string (line) & "<-", level => log_threshold + 7);

					-- The first line should be the headline with the schematic version:
					-- READ SCHEMATIC HEADLINE:

					-- EESchema Schematic File Version x
					
					if not schematic_version_valid then
						check_header (line); 
						-- sets schematic_version_valid true.
						-- aborts program if version not supported
					end if;

					-- READ SHEET HEADER:

					--	EESchema Schematic File Version 2
					--	LIBS:nucleo_core-rescue
					--	LIBS:power
					--	LIBS:bel_connectors_and_jumpers
					--	LIBS:bel_primitives
					--	LIBS:bel_stm32
					--	LIBS:nucleo_core-cache
					--	EELAYER 25 0
					--	EELAYER END

					if not sheet_header_entered then
						if get_field_from_line (f (line,1), 1, latin_1.colon) = schematic_library then
							sheet_header_entered := true;
							lines.append (line);
						end if;
					else -- we are inside the sheet header and wait for the footer
						if field_count (line) = 2 then
							if f (line,1) = schematic_eelayer 
								and f (line,2) = schematic_eelayer_end then
									sheet_header_entered := false;
									lines.append (line);
									make_sheet_header (lines);
									clear (lines); -- clean up line collector
							else
								lines.append (line);
							end if;
						end if;
					end if;
					
					-- READ DESCRIPTION:

					-- $Descr A4 11693 8268
					-- encoding utf-8
					-- Sheet 5 8
					-- Title ""
					-- Date ""
					-- Rev ""
					-- Comp ""
					-- Comment1 ""
					-- Comment2 ""
					-- Comment3 ""
					-- Comment4 ""
					-- $EndDescr
					
					if not description_entered then
						if f (line,1) = schematic_description_header then -- $Descr A4 11693 8268
							description_entered := true; -- we are entering the sheet description

							lines.append (line);
						end if;
					else -- we are inside the description
						if f (line,1) = schematic_description_footer then -- $EndDescr
							description_entered := false; -- we are leaving the description
							description_processed := true;

							make_drawing_frame (lines, log_threshold + 1);
							clear (lines); -- clean up line collector
						else
							lines.append (line);
						end if;
					end if;

					-- Read hierarchical GUI sheets (if there has been a total sheet count greater 1 detected earlier).
					-- A hierachical GUI sheet displays a hierarchical sheet as a black box with its ports.
					-- It serves as link between a hierachical net and the parent module.
					-- Rightly said this is the black box representation of a submodule. 
					-- So in the following we refer to them as "submodule".
					-- A submodule (sheet) section example:
					
					-- $Sheet
					-- S 4050 5750 1050 650 
					-- U 58A73B5D
					-- F0 "Sheet58A73B5C" 58
					-- F1 "morpho_test.sch" 58
					-- F2 "SENSOR_GND" U R 2250 3100 60 
					-- F3 "SENSOR_VCC" I L 1350 3250 60 
					-- $EndSheet

					if sheet_count_total > 1 then -- read hierarchical GUI sheets
						if not sheet_description_entered then
							if f (line,1) = schematic_sheet_header then -- $Sheet
								sheet_description_entered := true;
							end if;
						else -- we are inside a sheet description
							if f (line,1) = schematic_sheet_footer then -- $EndSheet
								sheet_description_entered := false; -- we are leaving the sheet description

								make_gui_sheet (lines, log_threshold + 1);
								clear (lines);
							else
								lines.append (line);
							end if;
						end if;
					end if;

					-- Further parts of the file can be read IF the description has been processed before (see above)
					if description_processed then
						
						-- READ NET SEGMENTS

						-- Wire Wire Line
						-- 2250 3100 2400 3100 

						if not net_segment_entered then
							if net_segment_header (line) then
								net_segment_entered := true;
							end if;
						else
							net_segment_entered := false; -- we are leaving a net segment
							lines.append (line);
							make_net_segment (lines, log_threshold + 1);
							clear (lines);
						end if;

						-- READ NET JUNCTIONS 
						
						-- Connection ~ 4650 4600

						if junction_header (line) then
							make_junction (line, log_threshold + 1);
						end if;
							
						-- READ SIMPLE NET LABELS (they do not have a tag, but just a text) 

						-- Text Label 2350 3250 0 60 ~ 0
						-- TOP_VCC

						if not simple_label_entered then
							if simple_label_header (line) then
								simple_label_entered := true;
								lines.append (line);
							end if;
						else
							simple_label_entered := false; -- we are leaving a simple label
							lines.append (line);
							make_simple_label (lines, log_threshold + 1);
							clear (lines);
						end if;
						
						-- READ TAG NET-LABELS (global or hierarchical)

						-- Text GLabel 4700 3200 1    60   UnSpc ~ 0
						-- DRV_1

						if not tag_label_entered then
							if tag_label_header (line) then
								tag_label_entered := true;
								lines.append (line);
							end if;
						else
							tag_label_entered := false; -- we are leaving a tag label
							lines.append (line);
							make_tag_label (lines, log_threshold + 1);
							pac_lines_of_file.clear (lines);
						end if;

						-- READ NOTES 

						-- "Text Notes 3400 2800 0 60 Italic 12"
						-- "ERC32 Test Board"
						
						if not note_entered then
							if text_note_header (line) then
								note_entered := true; -- we are entering a note
								lines.append (line);
							end if;
						else 
							note_entered := false; -- we are leaving a note
							lines.append (line);
							make_text_note (lines, log_threshold + 1);
							clear (lines);
						end if;
						
						-- READ COMPONENTS
						-- Once a component header ($Comp) found, set component_entered flag. This indicates we are inside a component section.
						-- Inside the component section, we process its content until the component footer ($EndComp) is found.
						-- Some entries of the component section are relevant for the whole component. Some entries are unit specific.
						-- The component section looks like this example:
						
						-- $Comp
						-- L 74LS00 U1		-- component specific
						-- U 4 1 5965E676	-- unit specific
						-- P 4100 4000		-- unit specific
						-- F 0 "U1" H 4100 4050 50  0000 C CNN		-- text fields
						-- F 1 "74LS00" H 4100 3900 50  0000 C CNN	
						-- F 2 "bel_ic:S_SO14" H 4100 4000 50  0001 C CNN
						-- F 3 "" H 4100 4000 50  0001 C CNN
						-- 	4    4100 4000		-- CS: same as x/y pos ?

						--  1    0    0  -1  -- orientation 0,   mirror normal
						--  0   -1   -1   0  -- orientation 90,  mirror normal
						-- -1    0    0   1  -- orientation 180, mirror normal 
						-- 	0    1    1   0  -- orientation -90, mirror normal  

						-- 	1    0    0   1  -- orientation 0,   mirror --
						--  0   -1    1   0  -- orientation 90,  mirror -- 
						-- -1    0    0  -1  -- orientation 180, mirror -- 
						--  0    1   -1   0  -- orientation -90, mirror -- 

						-- -1    0    0  -1  -- orientation 0,   mirror |
						--  0    1   -1   0  -- orientation 90,  mirror |
						--  1    0    0   1  -- orientation 180, mirror |
						--  1    0    0   1  -- orientation -90, mirror |
						-- $EndComp

						if not component_entered then
							if component_header (line) then
								component_entered := true;
							end if;
						else -- we are inside the component and wait for the component footer ($EndComp)
							if component_footer (line) then
								component_entered := false; -- we are leaving the component

								make_component (lines, log_threshold + 1);
								clear (lines);
							else -- read lines of unit/component
								lines.append (line);
							end if;
						
						end if;

						-- READ NO-CONNECT-FLAGS

						-- NoConn ~ 5000 3900
						
						if no_connection_header (line) then
							make_no_connection (line);
						end if;
					end if;

			end case;

		end loop;

		close (schematic_handle);
		log_indentation_down;
		--log (text => "reading complete. closing schematic file " &
		--	 to_string (current_schematic.sheet.file) & " ...", log_threshold);

		-- From the wild list of net segments, assemble net segments to anonymous strands.
		-- A strand is: all net segments connected with each other by their start or end points.
		build_anonymous_strands (log_threshold + 1);

		-- All anonymous strands must be given a name. The name is enforced by the a net label.
		-- (The fact that power-put ports enforce a name also, is cared for later on netlist generation.)
		-- The first label found on the strand sets the strand name and scope. 
		-- Other labels on the strand are checked for their name only. 
		-- If the name differs from the net name set earlier, a warning is output. 
		-- Strands without label remain anonymous. Their name is assigned by using the notation "N$".
		-- The strands are finally appended to the strands of the current module (see spec. of type_module.strands).
		associate_net_labels_with_anonymous_strands (log_threshold + 1);

	else
		log (ERROR, "schematic file '" & to_string (current_schematic.sheet.file) & "' not found !",
			console => true);
		raise constraint_error;
	end if;

	return hierarchic_sheet_file_names;

	exception
		when others =>
			error_in_schematic_file (line);
			raise;					

end read;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
