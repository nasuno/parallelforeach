
proc _test2() {
	 x_new_thread('myThread', closure(){
		 set_interval(250, closure(
	 		_tteesstt()
			))
	 })
}
proc _testThreads() {
	 x_new_thread('myThread', closure(){
	 		_tteesstt()			
	 })
}

proc _production() {
	@isMyLastIntervalComplete = 1 #1 is go
	export('namespaceOfIntervals.isMyLastIntervalComplete', @isMyLastIntervalComplete)
	set_interval(400, closure(
		_tteesstt()
	))
}



proc _productionSingleFrame() {
	@isMyLastIntervalComplete = 1 #1 is go
	export('namespaceOfIntervals.isMyLastIntervalComplete', @isMyLastIntervalComplete)

	_tteesstt()
}


proc _production_old() {
	@isMyLastIntervalComplete = 1 #1 is go
	export('namespaceOfIntervals.isMyLastIntervalComplete', @isMyLastIntervalComplete)
	set_interval(500, closure(
		_tteesstt()	
		sleep(1)
	))
}

proc _exmpl() {
	
}


proc _lookingTest() {	
	@isMe = pinfo()
	@thirdString = @isMe[1]
	@secondString = concat('Observor{',@thirdString[0],',',@thirdString[1],',',@thirdString[2]'}Observor')
	assign(@possibleLocationLookedAt, pcursor('nasuno'))
	@newString = concat(@secondString,'Observing{'@possibleLocationLookedAt[0],',',@possibleLocationLookedAt[1],',',@possibleLocationLookedAt[2],'}Observing')
}

proc _tteesstt() {
	assign(@shouldIstillWait, import('namespaceOfIntervals.isMyLastIntervalComplete'))

	if(@shouldIstillWait == 1){
		@letsNotGo = 2 #1 is go
		export('namespaceOfIntervals.isMyLastIntervalComplete', @letsNotGo)
		#===

	@stuck = 0
	while(@stuck < 1){
		@flagValue = _read_CH_flag()
		@indexKEYfromCoords = 'new'
		if(_areThereMoreThanNocoordsRows() == 'x') {
			try {
				@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
				assign(@result, unsafe_query(@profile, 'SELECT New_String FROM coords'))
				foreach(@row in @result) {@preProcessedString = @row['New_String']}

				@indexKEYfromCoords = concat(array_get(@preProcessedString[5]),array_get(@preProcessedString[6]))
				if(@flagValue == @indexKEYfromCoords) {
					if(string_ends_with(@preProcessedString, 'END')) {
						assign(@splitStringArray, split('%', @preProcessedString))
						@nearingIndexString = array_get(@splitStringArray[0])
						@stripENDfromThis = array_get(@splitStringArray[1])
						
						assign(@lastSplitStringArray, split('BEGIN', @nearingIndexString))
						@newArray = split('END', @stripENDfromThis)

						@INDEXstring = array_get(@lastSplitStringArray[1])
						@COORDINATESstring = array_get(@newArray[0])
						###msg("vv flagValue")
						###msg(@flagValue)
						_splitToBlackAndWhite(@COORDINATESstring)
						_incrementFlag(@flagValue)
						@stuck = 99
					}
				}
				###msg("vv indexKEYfromCoords")
				###msg(@indexKEYfromCoords)
				}catch(SQLException @sqlE) {
					#do nothing
					###msg(@sqlE)
				}
			}
	} #end of while

		#===
		@letsGo = 1
		export('namespaceOfIntervals.isMyLastIntervalComplete', @letsGo)
	} else {
		#do nothing
	}


}


proc _tteessttbak() {
	assign(@shouldIstillWait, import('namespaceOfIntervals.isMyLastIntervalComplete'))

	if(@shouldIstillWait == 1){
		@letsNotGo = 2 #1 is go
		export('namespaceOfIntervals.isMyLastIntervalComplete', @letsNotGo)
		#===

	@stuck = 0
	while(@stuck < 1){
		@flagValue = _read_CH_flag()
		@indexKEYfromCoords = 'new'
		if(_areThereMoreThanNocoordsRows() == 'x') {
			try {
				@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
				assign(@result, unsafe_query(@profile, 'SELECT New_String FROM coords'))
				foreach(@row in @result) {@preProcessedString = @row['New_String']}

				@indexKEYfromCoords = concat(array_get(@preProcessedString[5]),array_get(@preProcessedString[6]))
				if(@flagValue == @indexKEYfromCoords) {
					if(string_ends_with(@preProcessedString, 'END')) {
						assign(@splitStringArray, split('%', @preProcessedString))
						@nearingIndexString = array_get(@splitStringArray[0])
						@stripENDfromThis = array_get(@splitStringArray[1])
						
						assign(@lastSplitStringArray, split('BEGIN', @nearingIndexString))
						@newArray = split('END', @stripENDfromThis)

						@INDEXstring = array_get(@lastSplitStringArray[1])
						@COORDINATESstring = array_get(@newArray[0])
						###msg("vv flagValue")
						###msg(@flagValue)
						_splitToBlackAndWhite(@COORDINATESstring)
						_incrementFlag(@flagValue)
						@stuck = 99
					}
				}
				###msg("vv indexKEYfromCoords")
				###msg(@indexKEYfromCoords)
				}catch(SQLException @sqlE) {
					#do nothing
					###msg(@sqlE)
				}
			}
	} #end of while

		#===
		@letsGo = 1
		export('namespaceOfIntervals.isMyLastIntervalComplete', @letsGo)
	} else {
		#do nothing
		sleep(0.25)
	}


}

proc _read_CH_flag() {
	if(_areThereMoreThanNoCHFLAGRows() == 'x') {
		try {
			@profile = array(type: 'sqlite', file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
			assign(@result, unsafe_query(@profile, 'SELECT CH_Flag FROM Did_CH_UpdateMe'))
			foreach(@row in @result) {@final = @row['CH_Flag']}
			
			@finalString = concat(array_get(@final[0]),array_get(@final[1]))
			}catch(SQLException @sqlE) {
				#do nothing
			}
			@newSubString = substr(@finalString, 0, 2)
			switch(@newSubString){
				case '01':
					return('01')
				case '02':
					return('02')
				case '03':
					return('03')
				case '04':
					return('04')
				case '05':
					return('05')
				case '06':
					return('06')
				case '07':
					return('07')
				case '08':
					return('08')
				case '09':
					return('09')
				case '10':
					return('10')
				case 'aa':
					return('aa')
				case 'nope':
					return('bb')
				case '':
					return('xx')
			}
	}
	return('xx')
}

proc _incrementFlag(@oldFlagValue) {	
	@isMe = pinfo()
	@thirdString = @isMe[1]
	@secondString = concat('Observor{',@thirdString[0],',',@thirdString[1],',',@thirdString[2]'}Observor')
	assign(@possibleLocationLookedAt, pcursor('nasuno'))
	@newString = concat(@secondString,'Observing{'@possibleLocationLookedAt[0],',',@possibleLocationLookedAt[1],',',@possibleLocationLookedAt[2],'}Observing')


	@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
	unsafe_query(@profile, 'DELETE FROM Did_CH_UpdateMe')

	@stuck = 0
	while(@stuck < 1){
		if(_areThereMoreThanNoCHFLAGRows() == '0') {
			@stuck = 99
		}
		@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
		switch(@oldFlagValue){
			case '01':
				@newFlagValue = '02'
			case '02':
				@newFlagValue = '03'
			case '03':
				@newFlagValue = '04'
			case '04':
				@newFlagValue = '05'
			case '05':
				@newFlagValue = '06'
			case '06':
				@newFlagValue = '07'
			case '07':
				@newFlagValue = '08'
			case '08':
				@newFlagValue = '09'
			case '09':
				@newFlagValue = '10'
			case '10':
				@newFlagValue = '01'
			case 'aa':
				@newFlagValue = 'aa'
			case 'nope':
				@newFlagValue = 'nope'
			case '':
	#			msg("does this happen?")
				@newFlagValue = "empty"
		}
		@concatednewFlagValue = concat(@newFlagValue,@newString)
		unsafe_query(@profile, 'INSERT INTO `Did_CH_UpdateMe` (`CH_Flag`) VALUES('?')', @concatednewFlagValue)
	}
}

proc _areThereMoreThanNoCHFLAGRows() {
	@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
	assign(@result, unsafe_query(@profile, 'SELECT COUNT( * ) FROM Did_CH_UpdateMe'))
	@compareWith = '{COUNT( * ): 0}'
	if(array_get(@result[0]) == @compareWith) {
		return('0')  }
	return('x')
}

proc _areThereMoreThanNocoordsRows() {
	@profile = array(type: sqlite, file: '/home/user/bt/plugins/CommandHelper/RamDiskLocation/BlockUpdates.sqlite')
	assign(@result, unsafe_query(@profile, 'SELECT COUNT( * ) FROM coords'))
	@compareWith = '{COUNT( * ): 0}'
	if(array_get(@result[0]) == @compareWith) {
		
		# Experiment
	#	msg("{COUNT( * ): 0}       There is no coord data :(")
		set_timeout(25, closure(){
			return('0')  ;
   		});

		}
	# msg("didn't match, we get to go")
	return('x')
}

proc _splitToBlackAndWhite(@theString) {
	@secondPart = substr(@theString, string_position(@theString, '#'),)
	@firstPart = substr(@theString, 0, string_position(@theString, '#'))
	_splitcolerase(@firstPart)
	_splitcol(@secondPart)
}

proc _splitcolerase(@longstring) {
	@stringtosplit = @longstring
	assign(@splitstring, split(";", @stringtosplit))
	foreach(@splitstring, @i, _splitseterase(@i))
}

proc _splitcol(@longstring) {
	@killSeperator = @longstring
	assign(@stringtosplit, replace(@killSeperator, "#", ""))
	assign(@splitstring, split(";", @stringtosplit))
	foreach(@splitstring, @i, _splitset(@i))
}

proc _splitseterase(@myvar) {
	@stringtosplit = @myvar
	assign(@splitstring, split(",", @stringtosplit))
	array @final = array(@splitstring[0], @splitstring[1], @splitstring[2], 'world')
	set_block(@final, 'air')
}

proc _splitset(@myvar) {
	@stringtosplit = @myvar
	assign(@splitstring, split(",", @stringtosplit))
	array @final = array(@splitstring[0], @splitstring[1], @splitstring[2], 'world')
	set_block(@final, 'glowstone')
}

































































































#===============================================================================
#
#   Sample external MethodScript include. To use these functions, uncomment them.
#
#===============================================================================

/**
 * Returns the distance between two blocks, or any other 3d points, for that matter.
 * @param array @arr1 The first point, expects an array of x, y, z
 * @param array @arr2 The second point, expects an array of x, y, z
 */
/*proc _3d_distance(array @arr1, array @arr2){
	return(
		floor(
			sqrt(
				  ((@arr2[0] - @arr1[0]) ** 2)
				+ ((@arr2[1] - @arr1[1]) ** 2)
				+ ((@arr2[2] - @arr1[2]) ** 2)
			)
		)
	);
}*/

/**
 * Given two blocks, iterates through all the blocks inside the cuboid, and calls the 
 * user defined closure on them. The used defined closure should accept 3 parameters,
 * the x, y, and z coordinates of the block.
 */
/*proc _iterate_cuboid(array @b1, array @b2, closure @callback){
	for(@x = min(@b1[0], @b2[0]), @x <= max(@b1[0], @b2[0]), @x++){
		for(@y = min(@b1[1], @b2[1]), @y <= max(@b1[1], @b2[1]), @y++){
			for(@z = min(@b1[2], @b2[2]), @z <= max(@b1[2], @b2[2]), @z++){
				execute(@x, @y, @z, @callback);
			}
		}
	}
}*/




























































































#===============================================================================
#
#   Sample external MethodScript include. To use these functions, uncomment them.
#
#===============================================================================

/**
 * Returns the distance between two blocks, or any other 3d points, for that matter.
 * @param array @arr1 The first point, expects an array of x, y, z
 * @param array @arr2 The second point, expects an array of x, y, z
 */
/*proc _3d_distance(array @arr1, array @arr2){
	return(
		floor(
			sqrt(
				  ((@arr2[0] - @arr1[0]) ** 2)
				+ ((@arr2[1] - @arr1[1]) ** 2)
				+ ((@arr2[2] - @arr1[2]) ** 2)
			)
		)
	);
}*/

/**
 * Given two blocks, iterates through all the blocks inside the cuboid, and calls the 
 * user defined closure on them. The used defined closure should accept 3 parameters,
 * the x, y, and z coordinates of the block.
 */
/*proc _iterate_cuboid(array @b1, array @b2, closure @callback){
	for(@x = min(@b1[0], @b2[0]), @x <= max(@b1[0], @b2[0]), @x++){
		for(@y = min(@b1[1], @b2[1]), @y <= max(@b1[1], @b2[1]), @y++){
			for(@z = min(@b1[2], @b2[2]), @z <= max(@b1[2], @b2[2]), @z++){
				execute(@x, @y, @z, @callback);
			}
		}
	}
}*/
