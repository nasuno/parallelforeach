Imports System.Collections.Concurrent
Imports System.IO
Imports System.Text
Imports System.Data.SQLite
Imports System.Threading
Imports System.Timers
Imports Timer = System.Timers.Timer

Module Program
    Public userCoordinates As (Integer, Integer, Integer)
    Dim objectDictionary As New ConcurrentDictionary(Of Integer, MyObject)
    Dim uniqId As Integer = 0
    'Public panelRecord As New Dictionary(Of String, Single)   ' ** i commented THIS I THINK IT'S REDUNDENT ** BETTER chec it
    Dim objectsIntersectionDict As New ConcurrentDictionary(Of Integer, Tuple(Of (X As Double, Y As Double, Z As Double), (X As Double, Y As Double, Z As Double))) ' objID whiteCoordinates(current objID coords) blackCoordinates(past objID coords)



    Public Class Program
        Public Shared Sub Main()

            ' =Load 3D objects=
            CreateAndAddPregeneratedObjects()
            CreateUCSIcon()
            'CreateBarExtensions()
            Console.WriteLine("3D objects loaded")


            Dim produceWorker As New ComponentModel.BackgroundWorker
            Dim doneEvent As New AutoResetEvent(False)
            produceWorker.WorkerSupportsCancellation = True
            AddHandler produceWorker.DoWork,
                Sub(sender As Object, e As ComponentModel.DoWorkEventArgs)
                    While Not produceWorker.CancellationPending ' Infinite loop
                        ConsumeData() 'game loop
                    End While
                End Sub
            AddHandler produceWorker.RunWorkerCompleted,
                Sub(sender As Object, e As ComponentModel.RunWorkerCompletedEventArgs)
                    If e.Error IsNot Nothing Then
                        ' Handle the error
                        Console.WriteLine("e.Error")
                    ElseIf e.Cancelled Then
                        ' Handle cancellation
                        Console.WriteLine("e.Cancelled")
                    Else
                        ' Handle completion
                        Console.WriteLine("completion")
                    End If
                    doneEvent.Set() ' Signal doneEvent to unblock the main thread
                End Sub
            produceWorker.RunWorkerAsync()
            doneEvent.WaitOne() ' Main thread waits here until RunWorkerCompleted event signals
        End Sub

        Private Shared Sub ProduceData()
            While True 'Runs indefinitely
                For Each objectId In objectDictionary.Keys
                    Dim obj As MyObject = Nothing
                    If objectDictionary.TryGetValue(objectId, obj) Then
                        ' Just read the object's location without updating it
                        Dim currentLocation As Coordinates3D = obj.Location
                        ' ok you could writeline this.
                        ' * ProduceData nolonger does anything *
                        ' we can use it to effect the objects on another thread where our apps run?
                    End If
                Next
                Thread.Sleep(50)
            End While
        End Sub

        Public Shared Sub ConsumeData()

            'Encoding.RegisterProvider(CodePagesEncodingProvider.Instance)
            'Dim enc1252 = Encoding.GetEncoding(1252)
            'CreateBlockUpdatesDatabaseStructure()

            '=  apps  =
            'Dim DemoApplication As New DemoApp
            'Dim RWords As New WordGeneratorApp
            'Dim FrameCounter As New FrameCounterApp    'we will be using one of these demo apps for the 3d programs.

            '=end apps=

            Dim i As Integer = 0

            Dim doIGo As New Boolean
            doIGo = False

            While i < 1
                i += 1
                i -= 1

                Dim minDistanceCoordinates As New List(Of (Integer, Integer, Integer)) ' clear me when needed** *

                Dim flag As String
                Dim playerdata As String
                Dim newflagsvalue As List(Of String)
                newflagsvalue = tryToGetTheValue()
                flag = newflagsvalue(0)
                playerdata = newflagsvalue(1)

                Dim possibleCoordINDEXvalue As String = tryToGetTheCOORDINDEXValue().ToString ' ]*[ that's why it has to be one of the correct values to start. in the sqlite file. ]*[

                If flag = possibleCoordINDEXvalue Then
                    'do nothing
                Else

                    Select Case possibleCoordINDEXvalue '(a pre-assigned integer variable by you).
                        Case "01" To "11"
                            doIGo = True
                        Case "bb"
                            doIGo = True
                        Case Else
                            doIGo = False
                            'do nothing
                    End Select

                    If doIGo = True Then

                        fillUserCoordinates(playerdata)
                        'Console.WriteLine(objectDictionary.Count.ToString)

                        Dim bounds As New PanelBounds()

                        Dim stopwatch As Stopwatch = Stopwatch.StartNew()




                        Parallel.ForEach(objectDictionary.Keys, Sub(objectID)

                                                                    Dim obj As MyObject = Nothing
                                                                    If objectDictionary.TryGetValue(objectID, obj) Then

                                                                        ' Your processing logic here
                                                                        Dim x As Long = obj.Location.X
                                                                        Dim y As Long = obj.Location.Y
                                                                        Dim z As Long = obj.Location.Z

                                                                        Dim rayPnt As (Integer, Integer, Integer) = (x, y, z)

                                                                        Dim intersections As New Dictionary(Of String, Vector3D)

                                                                        Dim panels = PanelDataManager.Normals ' * Change name of PanelDataManager * FFS manager really?


                                                                        For Each panel In panels
                                                                            Dim intersect = GetIntersection(rayPnt, userCoordinates, panel.Item2, panel.Item3)
                                                                            ' Console.WriteLine("Intersection for {0}: {1}, Intersection exists: {2}", panel.Item1, intersect.Item1, intersect.Item2)

                                                                            If intersect.Item2 Then
                                                                                intersections.Add(panel.Item1, intersect.Item1)
                                                                            End If
                                                                        Next

                                                                        For Each panelName In intersections.Keys
                                                                            Dim panelIntersection = intersections(panelName)
                                                                            ' Console.WriteLine("{0} intersection point: {1}", panelName, panelIntersection)
                                                                        Next

                                                                        Dim intersectionsWithinBounds As New Dictionary(Of String, Vector3D)()

                                                                        For Each panelName In intersections.Keys
                                                                            Dim panelIntersection = intersections(panelName)
                                                                            Dim coords As String() = panelIntersection.ToString().Split(", ")

                                                                            Try
                                                                                Dim point As (Integer, Integer, Integer) = (CInt(coords(0)), CInt(coords(1)), CInt(coords(2)))
                                                                                Dim isWithinBounds As Boolean = bounds.IsPointWithinPanel(panelName, point)
                                                                                ' Console.WriteLine($"{panelName} intersection point {panelIntersection} is within bounds: {isWithinBounds}")

                                                                                If isWithinBounds Then
                                                                                    intersectionsWithinBounds.Add(panelName, panelIntersection)
                                                                                End If
                                                                            Catch e As OverflowException ' Handle the exception
                                                                                'Console.WriteLine("One of the coordinates is too large or too small to fit into an integer.")
                                                                                'Do nothing
                                                                            End Try

                                                                        Next

                                                                        For Each kvp In intersectionsWithinBounds
                                                                            ' Console.WriteLine($"Panel: {kvp.Key}, Intersection: {kvp.Value}")
                                                                        Next

                                                                        Dim points As Dictionary(Of String, (Integer, Integer, Integer)) = intersectionsWithinBounds.ToDictionary(Function(pair) pair.Key, Function(pair) pair.Value.ToIntTuple())
                                                                        Dim distances As Dictionary(Of String, Single) = points.ToDictionary(Function(pair) pair.Key, Function(pair) CalculateFastDistance(rayPnt, pair.Value))
                                                                        Dim minDistance As Single = Single.MaxValue
                                                                        Dim minDistanceCoordinate As (Integer, Integer, Integer) = (0, 0, 0)

                                                                        For Each distancePair In distances
                                                                            If distancePair.Value < minDistance Then
                                                                                minDistance = distancePair.Value
                                                                                minDistanceCoordinate = points(distancePair.Key)
                                                                            End If
                                                                        Next

                                                                        'Dim minDistanceString As String = $"{minDistanceCoordinate.Item1},{minDistanceCoordinate.Item2},{minDistanceCoordinate.Item3};"
                                                                        'Dim whiteCoordinatesBuilder As New StringBuilder(minDistanceString)
                                                                        'Dim blackCoordinatesBuilder As New StringBuilder("-370,73,-200;")

                                                                        Dim individualObject As (ObjID As Integer, X As Double, Y As Double, Z As Double) = (obj.UniqIdentifier, minDistanceCoordinate.Item1, minDistanceCoordinate.Item2, minDistanceCoordinate.Item3)

                                                                        If Not objectsIntersectionDict.ContainsKey(individualObject.ObjID) Then

                                                                            Dim added As Boolean = objectsIntersectionDict.TryAdd(individualObject.ObjID, New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), (0, 0, 0)))

                                                                            'objectsIntersectionDict.Add(individualObject.ObjID, (New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), (0, 0, 0))))
                                                                        Else
                                                                            Dim existingValues = objectsIntersectionDict(individualObject.ObjID).Item1 ' * 
                                                                            If existingValues.X <> individualObject.X Or existingValues.Y <> individualObject.Y Or existingValues.Z <> individualObject.Z Then
                                                                                Dim previousValue = existingValues ' *
                                                                                objectsIntersectionDict(individualObject.ObjID) = New Tuple(Of (Double, Double, Double), (Double, Double, Double))((individualObject.X, individualObject.Y, individualObject.Z), previousValue)
                                                                            End If
                                                                        End If






                                                                        Dim ticks As Long = stopwatch.ElapsedTicks
                                                                        Dim nanosecondsPerTick As Double = (1000000000.0 / Stopwatch.Frequency)
                                                                        Dim elapsedNanoseconds As Double = ticks * nanosecondsPerTick
                                                                        Dim elapsedMilliseconds As Double = elapsedNanoseconds / 1000000.0
                                                                        Console.WriteLine(elapsedMilliseconds)







                                                                    End If




                                                                End Sub)


                        ' HashSets to store unique current and previous coordinates explicitly.
                        Dim uniqueCurrentCoordinatesSet As New HashSet(Of String)
                        Dim uniquePreviousCoordinatesSet As New HashSet(Of String)

                        For Each item As KeyValuePair(Of Integer, Tuple(Of (X As Double, Y As Double, Z As Double), (X As Double, Y As Double, Z As Double)))
                                    In objectsIntersectionDict
                            ' Add the current coordinates as strings in the HashSet to ensure uniqueness.
                            uniqueCurrentCoordinatesSet.Add($"{item.Value.Item1.X},{item.Value.Item1.Y},{item.Value.Item1.Z}")

                            ' Add the previous coordinates as strings in the HashSet to ensure uniqueness.
                            uniquePreviousCoordinatesSet.Add($"{item.Value.Item2.X},{item.Value.Item2.Y},{item.Value.Item2.Z}")

                        Next

                        ' == difference engine work here ==   


                        Dim currentCoordinatesSb As New StringBuilder()
                        Dim previousCoordinatesSb As New StringBuilder()

                        For Each coordinate As String In uniqueCurrentCoordinatesSet ' * WHITE
                            If currentCoordinatesSb.Length > 0 Then
                                currentCoordinatesSb.Append(";")
                            End If

                            'If CheckDifference(coordinate, pixelType.white) Then
                            currentCoordinatesSb.Append(coordinate)
                            'End If

                        Next

                        For Each coordinate As String In uniquePreviousCoordinatesSet ' * BLACK
                            If previousCoordinatesSb.Length > 0 Then
                                previousCoordinatesSb.Append(";")
                            End If

                            'If CheckDifference(coordinate, pixelType.black) Then
                            previousCoordinatesSb.Append(coordinate)
                            'End If

                        Next

                        Dim coordinatesTuple2 As Tuple(Of StringBuilder, StringBuilder) = New Tuple(Of StringBuilder, StringBuilder)(currentCoordinatesSb, previousCoordinatesSb)






                        allFrameTuples.Add(coordinatesTuple2)






                        'delete coordinates
                        Dim PathAndNameOfBlockUpdatesSQLite As String = String.Concat(IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation"), "/BlockUpdates.sqlite")
                        Using myConnection As SQLiteConnection = New SQLiteConnection()
                            Dim myConnectionString As String
                            myConnectionString = "Data Source=" + PathAndNameOfBlockUpdatesSQLite + ";Version=3; Journal Mode = OFF;"
                            myConnection.ConnectionString = myConnectionString
                            myConnection.Open()
                            Using tx = myConnection.BeginTransaction()
                                Using cmd = myConnection.CreateCommand()
                                    Try
                                        Dim myColumnData = "DELETE FROM coords "
                                        cmd.CommandText = myColumnData
                                        cmd.ExecuteNonQuery()
                                    Catch ex As Exception
                                        Console.WriteLine(ex.ToString)
                                    End Try
                                End Using
                                tx.Commit()
                            End Using
                        End Using

                        InsertSQLiteValues("coords", testBlocksPlusBlackAndWhite(newflagsvalue, totalFrameTuple))

                        allFrameTuples.Clear()

                        '#==update apps below==#

                        'RWords.submitSpatialZones() 'would be nice if, could tell all apps to submit sz'z.
                        'FrameCounter.submitSpatialZones()

                        '#====================#
                    Else
                    End If

                End If

            End While
        End Sub

    End Class



    ' ============== MOVE TO 3D APP ================
    Sub CreateUCSIcon()
        Dim i As Integer
        For i = 0 To 150 Step 1
            AddMyObjectToFactory(i, 0, 0) ' Points on X-axis
            AddMyObjectToFactory(0, i, 0) ' Points on Y-axis
            AddMyObjectToFactory(0, 0, i) ' Points on Z-axis
        Next i
    End Sub
    Sub CreateBarExtensions()
        AddMyObjectToFactory(-129, 160, -69)
        AddMyObjectToFactory(-128, 160, -69)
        AddMyObjectToFactory(-127, 160, -69)
        AddMyObjectToFactory(-126, 160, -69)
        AddMyObjectToFactory(-125, 160, -69)
        Dim i As Integer
        For i = -124 To -50
            AddMyObjectToFactory(i, 160, -69)
        Next
        For i = 161 To 165
            AddMyObjectToFactory(-50, i, -69)
        Next
        For i = 159 To 155 Step -1
            AddMyObjectToFactory(-50, i, -69)
        Next
        AddMyObjectToFactory(-371, 160, -69)
        AddMyObjectToFactory(-372, 160, -69)
        AddMyObjectToFactory(-373, 160, -69)
        AddMyObjectToFactory(-374, 160, -69)
        AddMyObjectToFactory(-375, 160, -69)
        For i = -376 To -450 Step -1
            AddMyObjectToFactory(i, 160, -69)
        Next
    End Sub
    Sub CreateAndAddPregeneratedObjects()
        AddMyObjectToFactory(0, 0, 0) ' zero point of all UCS axes
#Region "one_coord_per_panel"
        AddMyObjectToFactory(-258, 147, 339)
        AddMyObjectToFactory(-664, 147, -78)
        AddMyObjectToFactory(-255, 147, -493)
        AddMyObjectToFactory(168, 147, -74)
        AddMyObjectToFactory(-250, 300, -78)
        AddMyObjectToFactory(-250, -75, -78)
#End Region
    End Sub
    Sub AddMyObjectToFactory(x As Integer, y As Integer, z As Integer)
        Dim key As Integer = GetNextUniqId()
        Dim location As New Coordinates3D(x, y, z)
        Dim obj As New MyObject(location, key)
        objectDictionary.TryAdd(key, obj)
    End Sub
    Function GetNextUniqId() As Integer
        uniqId += 1
        Return uniqId
    End Function
    ' ================ END OF MOVE TO 3D APP ===================

    Public Class Coordinates3D
        Public Property X As Long
        Public Property Y As Long
        Public Property Z As Long

        Public Sub New(x As Long, y As Long, z As Long)
            Me.X = x
            Me.Y = y
            Me.Z = z
        End Sub
    End Class

    Public Class MyObject
        Public Property UniqIdentifier As Integer
        Public Property Location As Coordinates3D
        Private locationUpdateTimer As Timer

        Public Sub New(location As Coordinates3D, key As Integer)
            Me.Location = location
            Me.UniqIdentifier = key
            InitializeLocationUpdateTimer()
        End Sub

        Private Sub InitializeLocationUpdateTimer()
            locationUpdateTimer = New Timer(1000)
            AddHandler locationUpdateTimer.Elapsed, AddressOf UpdateLocation
            locationUpdateTimer.AutoReset = True
            locationUpdateTimer.Enabled = True
        End Sub

        Private Sub UpdateLocation(sender As Object, e As ElapsedEventArgs)
            ' Simulate location change
            Dim newLocation As New Coordinates3D(Me.Location.X, Me.Location.Y, Me.Location.Z)
            Me.Location = newLocation
        End Sub
    End Class


#Region "meat"
    Function GetIntersection(rayPnt As (Long, Long, Long), userCoordinates As (Long, Long, Long),
                         planeNormal As (Long, Long, Long), planePoint As (Long, Long, Long)) As (Vector3D, Boolean)
        Dim rp = New Vector3D(rayPnt.Item1, rayPnt.Item2, rayPnt.Item3)
        Dim observer = New Vector3D(userCoordinates.Item1, userCoordinates.Item2, userCoordinates.Item3)
        Dim pn = New Vector3D(planeNormal.Item1, planeNormal.Item2, planeNormal.Item3)
        Dim pp = New Vector3D(planePoint.Item1, planePoint.Item2, planePoint.Item3)

        Dim rv = rp - observer

        Dim ip = IntersectPoint(rv, rp, pn, pp)

        Dim intersectionExists = (ip IsNot Nothing)

        Return (ip, intersectionExists)

    End Function

    Function CalculateFastDistance(point1 As (Integer, Integer, Integer), point2 As (Integer, Integer, Integer)) As Single
        Dim xDiff As Integer = point2.Item1 - point1.Item1
        Dim yDiff As Integer = point2.Item2 - point1.Item2
        Dim zDiff As Integer = point2.Item3 - point1.Item3

        Return MathF.Sqrt(xDiff * xDiff + yDiff * yDiff + zDiff * zDiff)
    End Function

    Function IntersectPoint(rayVector As Vector3D, rayPoint As Vector3D, planeNormal As Vector3D, planePoint As Vector3D) As Vector3D
        Dim diff = rayPoint - planePoint
        Dim prod1 = diff.Dot(planeNormal)
        Dim prod2 = rayVector.Dot(planeNormal)
        Dim prod3 = prod1 / prod2
        Return rayPoint - rayVector * prod3
    End Function














    Public Class PanelBounds
        Private ReadOnly _precalculatedBounds As Dictionary(Of String, (Integer, Integer, Integer, Integer, Integer, Integer))
        ' the holo_dt exists and then the loop may use it,...
        ' the loop derives its ephermiral copy from the real- from pre loop definition.
        Public Sub New()
            Dim panels = PanelDataManager.PanelCorners ' ** RENAME THIS **
            _precalculatedBounds = panels.ToDictionary(Function(panel) panel.Item1, Function(panel) CalculateMinMaxBounds(panel.Item2, panel.Item3))
        End Sub

        Private Function CalculateMinMaxBounds(corner1 As (Integer, Integer, Integer), corner2 As (Integer, Integer, Integer)) As (Integer, Integer, Integer, Integer, Integer, Integer)
            Dim minX As Integer = Math.Min(corner1.Item1, corner2.Item1)
            Dim maxX As Integer = Math.Max(corner1.Item1, corner2.Item1)
            Dim minY As Integer = Math.Min(corner1.Item2, corner2.Item2)
            Dim maxY As Integer = Math.Max(corner1.Item2, corner2.Item2)
            Dim minZ As Integer = Math.Min(corner1.Item3, corner2.Item3)
            Dim maxZ As Integer = Math.Max(corner1.Item3, corner2.Item3)

            Return (minX, maxX, minY, maxY, minZ, maxZ)
        End Function

        Public Function IsPointWithinPanel(panelName As String, point As (Integer, Integer, Integer)) As Boolean
            If _precalculatedBounds.ContainsKey(panelName) Then
                Dim bounds = _precalculatedBounds(panelName)
                Return (point.Item1 >= bounds.Item1) AndAlso (point.Item1 <= bounds.Item2) AndAlso (point.Item2 >= bounds.Item3) AndAlso (point.Item2 <= bounds.Item4) AndAlso (point.Item3 >= bounds.Item5) AndAlso (point.Item3 <= bounds.Item6)
            Else
                Throw New ArgumentException($"Invalid panel name: {panelName}")
            End If
        End Function

    End Class

    Class Vector3D
        Private ReadOnly x As Double
        Private ReadOnly y As Double
        Private ReadOnly z As Double

        Sub New(nx As Double, ny As Double, nz As Double)
            x = nx
            y = ny
            z = nz
        End Sub

        Public Function Dot(rhs As Vector3D) As Double
            Return x * rhs.x + y * rhs.y + z * rhs.z
        End Function

        Public Shared Operator +(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x + b.x, a.y + b.y, a.z + b.z)
        End Operator

        Public Shared Operator -(ByVal a As Vector3D, ByVal b As Vector3D) As Vector3D
            Return New Vector3D(a.x - b.x, a.y - b.y, a.z - b.z)
        End Operator

        Public Shared Operator *(ByVal a As Vector3D, ByVal b As Double) As Vector3D
            Return New Vector3D(a.x * b, a.y * b, a.z * b)
        End Operator

        Public Overrides Function ToString() As String
            Return String.Format("{0:F}, {1:F}, {2:F}", x, y, z)
        End Function

        Public Function ToIntTuple() As (Integer, Integer, Integer)
            Return (CInt(Fix(x)), CInt(Fix(y)), CInt(Fix(z)))
        End Function
    End Class
#End Region

#Region "Sqlite"
    Function tryToGetTheValue()

        Dim places As New List(Of String)
yolo:
        Dim indexTest As String = RetrieveCHFlag()
        Dim playerData As String = indexTest
        playerData = playerData.Remove(0, 2)

        Dim startIndex As Integer = 0
        Dim length As Integer = 2
        Dim substring As String = indexTest.Substring(startIndex, length)
        Select Case substring
            Case "01"
                places.Add("01")
                places.Add(playerData.ToString)
                Return places
            Case "02"
                places.Add("02")
                places.Add(playerData.ToString)
                Return places
            Case "03"
                places.Add("03")
                places.Add(playerData.ToString)
                Return places
            Case "04"
                places.Add("04")
                places.Add(playerData.ToString)
                Return places
            Case "05"
                places.Add("05")
                places.Add(playerData.ToString)
                Return places
            Case "06"
                places.Add("06")
                places.Add(playerData.ToString)
                Return places
            Case "07"
                places.Add("07")
                places.Add(playerData.ToString)
                Return places
            Case "08"
                places.Add("08")
                places.Add(playerData.ToString)
                Return places
            Case "09"
                places.Add("09")
                places.Add(playerData.ToString)
                Return places
            Case "10"
                places.Add("10")
                places.Add(playerData.ToString)
                Return places
            Case "aa"
                places.Add("aa")
                places.Add(playerData.ToString)
                Return places
            Case "nope"
                places.Add("bb")
                places.Add(playerData.ToString)
                Return places
            Case Else   'Today is: 12/11/2012 12:00:00 AM
                Console.WriteLine("YOLO")
                Console.WriteLine("Current Time: ")
                Dim dt As Date = Now
                Console.WriteLine("Time is: {0}", dt)
                GoTo yolo
        End Select

    End Function


    Function RetrieveCHFlag() As String
        Dim result As String = "nope"
        Dim PathAndNameOfBlockUpdatesSQLite As String = Path.Combine(Directory.GetCurrentDirectory(), "RamDiskLocation", "BlockUpdates.sqlite")
        'Console.WriteLine(PathAndNameOfBlockUpdatesSQLite)
        Try
            Using sqlite_conn As New SQLiteConnection($"Data Source={PathAndNameOfBlockUpdatesSQLite};Version=3;New=True;")
                sqlite_conn.Open()

                Using sqlite_cmd As New SQLiteCommand("SELECT * FROM Did_CH_UpdateMe", sqlite_conn)
                    Using sqlite_datareader As SQLiteDataReader = sqlite_cmd.ExecuteReader()

                        While (sqlite_datareader.Read())
                            Dim textReader As String = sqlite_datareader.GetString(0)
                            result = textReader.ToString
                            ' Console.WriteLine(result) '    ***
                        End While

                    End Using
                End Using
            End Using

        Catch ex As Exception
            Console.WriteLine(ex.Message)
            'Console.ReadLine() ' *****
        End Try

        Return result
    End Function



    Function RetrieveCoord_INDEX_Value() As String
        Dim result As String = "nope"
        Dim PathAndNameOfBlockUpdatesSQLite As String = Path.Combine(Directory.GetCurrentDirectory(), "RamDiskLocation", "BlockUpdates.sqlite")

        Try
            Using sqlite_conn As New SQLiteConnection($"Data Source={PathAndNameOfBlockUpdatesSQLite};Version=3;New=True;")
                sqlite_conn.Open()

                Using sqlite_cmd As New SQLiteCommand("SELECT * FROM coords", sqlite_conn)
                    Using sqlite_datareader As SQLiteDataReader = sqlite_cmd.ExecuteReader()

                        While (sqlite_datareader.Read())
                            Dim textReader As String = sqlite_datareader.GetString(0)
                            result = textReader.ToString
                        End While

                    End Using
                End Using
            End Using

        Catch ex As Exception
            Console.WriteLine(ex.Message)
        End Try

        Return result.Substring(5, 2)
    End Function

    Sub InsertSQLiteValues(Table As String, Value As String)
        Dim PathAndNameOfBlockUpdatesSQLite As String = IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation", "BlockUpdates.sqlite")

        If System.IO.File.Exists(PathAndNameOfBlockUpdatesSQLite) Then
            CreateSQLiteData(PathAndNameOfBlockUpdatesSQLite, Table, Value)
        End If

    End Sub



    Sub CreateSQLiteData(sqliteDB As String, tableName As String, dataValue As String)
        Dim myConnectionString As String = String.Format("Data Source={0};Version=3; Journal Mode = OFF;", sqliteDB)

        Using myConnection As SQLiteConnection = New SQLiteConnection(myConnectionString)
            myConnection.Open()
            Using tx = myConnection.BeginTransaction()
                Using cmd = myConnection.CreateCommand()
                    Dim myColumnData = $"INSERT INTO {tableName} VALUES (?);"
                    cmd.CommandText = myColumnData
                    cmd.Parameters.AddWithValue("?", dataValue)
                    Try
                        cmd.ExecuteNonQuery()
                    Catch ex As Exception
                        Console.WriteLine("An error occurred: " & ex.ToString)
                    End Try
                End Using
                tx.Commit()
            End Using
        End Using
    End Sub



    Sub CreateBlockUpdatesDatabaseStructure()
        Dim PathAndNameOfBlockUpdatesSQLite As String = String.Concat(IO.Path.Combine(IO.Path.GetFullPath(Directory.GetCurrentDirectory), "RamDiskLocation"), "/BlockUpdates.sqlite")

        If System.IO.File.Exists(PathAndNameOfBlockUpdatesSQLite) Then
            Console.WriteLine(" Old Database Found: " + PathAndNameOfBlockUpdatesSQLite + " Please reset.")
        Else
            Console.WriteLine("Not Found: " + PathAndNameOfBlockUpdatesSQLite)
            SQLiteConnection.CreateFile(PathAndNameOfBlockUpdatesSQLite)

            Dim BlockUpdates_coords_Tablename As String = "coords"
            Dim BlockUpdatesColumnName_forcoordstable As String = "New_String"
            Dim BlockUpdates_Did_VB_UpdateMe_Tablename As String = "Did_VB_UpdateMe"
            Dim BlockUpdatesColumnName_forDid_VB_UpdateMetable As String = "VB_Flag"
            Dim BlockUpdates_Did_CH_UpdateMe_Tablename As String = "Did_CH_UpdateMe"
            Dim BlockUpdatesColumnName_forDid_CH_UpdateMetable As String = "CH_Flag"
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_coords_Tablename, BlockUpdatesColumnName_forcoordstable)
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_Did_VB_UpdateMe_Tablename, BlockUpdatesColumnName_forDid_VB_UpdateMetable)
            CreateTable(PathAndNameOfBlockUpdatesSQLite, BlockUpdates_Did_CH_UpdateMe_Tablename, BlockUpdatesColumnName_forDid_CH_UpdateMetable)
        End If

    End Sub
    Sub CreateTable(sqliteDB As String, tableName As String, columnName As String)

        Using myConnection As SQLiteConnection = New SQLiteConnection()
            Dim myConnectionString As String
            myConnectionString = "Data Source=" + sqliteDB + ";Version=3; Journal Mode = OFF;"
            myConnection.ConnectionString = myConnectionString
            myConnection.Open()
            Using tx = myConnection.BeginTransaction()
                Using cmd = myConnection.CreateCommand()
                    Try
                        Dim myTableCreate = $"CREATE TABLE {tableName} ({columnName} text primary key) WITHOUT ROWID;"
                        cmd.CommandText = myTableCreate
                        cmd.ExecuteNonQuery()
                    Catch ex As Exception
                        Console.WriteLine(ex.ToString)
                    End Try
                End Using
                tx.Commit()
            End Using
        End Using

    End Sub

#End Region

#Region "Flag"

    Function tryToGetTheCOORDINDEXValue()

        Dim indexTest As String = RetrieveCoord_INDEX_Value()
        Select Case indexTest
            Case "01"
                Return "01"
            Case "02"
                Return "02"
            Case "03"
                Return "03"
            Case "04"
                Return "04"
            Case "05"
                Return "05"
            Case "06"
                Return "06"
            Case "07"
                Return "07"
            Case "08"
                Return "08"
            Case "09"
                Return "09"
            Case "10"
                Return "10"
            Case "aa"
                Return "aa"
            Case "nope"
                Return "bb"
            Case Else
                Return "xx"
        End Select

    End Function

    Sub fillUserCoordinates(playerData As String)

        Dim startAt = playerData.IndexOf("Observor{", 0) + 9
        Dim endAt = playerData.IndexOf(" }Observor", 0) '       
        Dim difference = endAt - startAt
        Dim unsplitObserverCoords As String = playerData.Substring(startAt, difference)
        Dim s As String = New String(unsplitObserverCoords)

        Dim haveFloat As String() = s.Split(New Char() {","c})
        userCoordinates.Item1 = CInt(haveFloat(0)) '    x
        userCoordinates.Item2 = CInt(haveFloat(1)) + 2 'y   +2 for player height. 
        userCoordinates.Item3 = CInt(haveFloat(2)) '    z

    End Sub
#End Region

#Region "InsertSQLiteValues"

    Public allFrameTuples As New List(Of Tuple(Of StringBuilder, StringBuilder))
    Public Record As New Dictionary(Of String, String)
    Function totalFrameTuple()

        Dim black As New StringBuilder
        For Each tupl In allFrameTuples
            If CheckAndUpdateState(tupl.Item2.ToString(), "black") Then
                black.Append(tupl.Item2)                ' Swap these to reverse the colors.
                black.Append(";")
            End If
        Next

        Dim white As New StringBuilder
        For Each tupl In allFrameTuples
            If CheckAndUpdateState(tupl.Item1.ToString(), "white") Then
                white.Append(tupl.Item1)
                white.Append(";")
            End If
        Next

        Dim amalgamationOfFrameTuples = New Tuple(Of StringBuilder, StringBuilder)(black, white)
        Return amalgamationOfFrameTuples

    End Function

    Function CheckAndUpdateState(key As String, color As String) As Boolean
        If Not Record.ContainsKey(key) Then
            ' State 1: Key not in dictionary, add key-value pair and return true
            Record.Add(key, color)
            Return True
        Else
            If Record(key) = color Then
                ' State 2: Key in dictionary and values match, return false
                Return False
            Else
                ' State 3: Key in dictionary and values don't match, update value and return true
                Record(key) = color
                Return True
            End If
        End If
    End Function

    Function testBlocksPlusBlackAndWhite(playerDataNeedsSplitUp As List(Of String), blackAndWhiteTuple As Tuple(Of StringBuilder, StringBuilder))

        Dim flag As String
        flag = playerDataNeedsSplitUp(0)

        Dim WhiteCoordinates As StringBuilder
        WhiteCoordinates = blackAndWhiteTuple.Item2
        Dim BlackCoordinates As StringBuilder
        BlackCoordinates = blackAndWhiteTuple.Item1

        Dim stringForFlashing As StringBuilder = New StringBuilder
        stringForFlashing.Append("BEGIN")
        stringForFlashing.Append(flag)
        stringForFlashing.Append("%")
        stringForFlashing.Append(BlackCoordinates.ToString)
        stringForFlashing.Append("-370,73,-200;-370,72,-200")   'black test

        stringForFlashing.Append("#")
        stringForFlashing.Append(WhiteCoordinates.ToString)
        stringForFlashing.Append("-370,75,-200;-370,74,-200")   'white test
        stringForFlashing.Append(";-64,68,-44")
        stringForFlashing.Append(";-80,70,-29")
        stringForFlashing.Append("END")

        Dim myDesiredOutput As String = stringForFlashing.ToString()
        Dim reducedToSingleLine As String = myDesiredOutput.Replace(vbCr, "").Replace(vbLf, "")
        Return reducedToSingleLine

    End Function
#End Region

#Region "Features"

    Function getlookedatcoords(pdata As List(Of String))

        'proper example: "10Observor{-313.7535383912487,114.11813668075226,-90.27644076604123 }ObservorObserving{ -261.0,213.0,-198.0}Observing"
        Dim flag As String
        flag = pdata(0)
        Dim playerdata As String
        playerdata = pdata(1)
        Dim startAt = playerdata.IndexOf("Observing{ ", 0) + 11
        Dim endAt = playerdata.IndexOf("}Observing", 0)
        Dim difference = endAt - startAt

        Dim unsplitPlayerCoords As String = playerdata.Substring(startAt, difference)
        Return unsplitPlayerCoords

    End Function


#End Region








    Public Class PanelDataManager
        Private CenterCoordinates As (centerX As Integer, centerY As Integer, centerZ As Integer)

        Public Sub New()
            Console.Write("Enter the X coordinate of the center: ")
            Dim centerX As Integer = Convert.ToInt32(Console.ReadLine())
            Console.Write("Enter the Y coordinate of the center: ")
            Dim centerY As Integer = Convert.ToInt32(Console.ReadLine())
            Console.Write("Enter the Z coordinate of the center: ")
            Dim centerZ As Integer = Convert.ToInt32(Console.ReadLine())

            CenterCoordinates = (centerX, centerY, centerZ)
        End Sub

        ' instantiate me in main



        Public Shared ReadOnly Property PanelCorners As (String, (Integer, Integer, Integer), (Integer, Integer, Integer))() = { ' which of these gets called first cause...
        ("North Panel", (-370, 74, -199), (-130, 234, -199)),
        ("East Panel", (-129, 74, -198), (-129, 234, 43)),
        ("South Panel", (-130, 74, 43), (-370, 234, 43)),
        ("West Panel", (-371, 74, 42), (-371, 234, -198)),
        ("Top Panel", (-370, 235, -198), (-130, 235, 43)),
        ("Bottom Panel", (-370, 73, -198), (-130, 73, 43))
    }

        Public Shared ReadOnly Property Normals As (String, (Integer, Integer, Integer), (Integer, Integer, Integer))() = { ' this one might grab its second values from the second values of PanelCorners.
        ("North Panel", (0, 0, 1), (-370, 74, -199)),
        ("East Panel", (33, 0, 0), (-129, 74, -198)),
        ("South Panel", (0, 0, 30), (-130, 74, 43)),
        ("West Panel", (-2, 0, 0), (-371, 74, 42)),
        ("Top Panel", (0, 34, 0), (-370, 235, -198)),
        ("Bottom Panel", (0, 34, 0), (-370, 73, -198))
    }
        ' this being pre-work for making the new code easy to integrate.



    End Class









End Module
