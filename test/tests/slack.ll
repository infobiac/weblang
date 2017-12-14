; ModuleID = 'WebLang'
source_filename = "<string>"

declare i32* @json(i32*)

declare i32 @puts(i8*)

declare i32 @floor(double)

declare i32 @strcmp(i8*, i8*)

declare i32* @jgets(i32*, i32*)

declare i32 @test(i8*)

declare i32* @post(i8*)

declare i32* @get(i8*)

declare i32* @json_double(double)

declare i8* @tostring(i32*)

declare i32* @json_string(i8*)

declare double @get_json_double(i32*)

declare i32* @json_array(i32**, i32)

declare i32* @create_arr_iter(i32*)

declare i32* @arr_next_elem(i32*, i32*)

declare i32* @get_json_from_array(i32*, i32)

define i32* @sendMsg(i32* %arg) {
entry:
  %0 = alloca i32*
  store i32* %arg, i32** %0
  %1 = alloca [169 x i8]
  store [169 x i8] c"{\22url\22:\22https://hooks.slack.com/services/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\22, \22payload\22: \22{'text':'Hey Lizzie, how are you? Check out this emoji :edwards:'}\22}\00", [169 x i8]* %1
  %2 = getelementptr inbounds [169 x i8], [169 x i8]* %1, i8 0, i8 0
  %3 = call i32* @json_string(i8* %2)
  %4 = alloca i32*
  store i32* %3, i32** %4
  %5 = load i32*, i32** %4
  %6 = call i32* @executeMsg(i32* %5)
  ret i32* %6
}

define i32* @executeMsg(i32* %arg) {
entry:
  %0 = alloca i32*
  store i32* %arg, i32** %0
  %1 = load i32*, i32** %0
  %2 = call i8* @tostring(i32* %1)
  %3 = call i32* @post(i8* %2)
  %4 = alloca [23 x i8]
  store [23 x i8] c"jordan es un buen nino\00", [23 x i8]* %4
  %5 = getelementptr inbounds [23 x i8], [23 x i8]* %4, i8 0, i8 0
  %6 = call i32* @json_string(i8* %5)
  %7 = call i8* @tostring(i32* %6)
  %8 = call i32 @puts(i8* %7)
  ret i32* %6
}

define i32 @main(i32* %argc, i8** %argv) {
entry:
  %0 = getelementptr inbounds i8*, i8** %argv, i32 1
  %1 = load i8*, i8** %0, align 1
  %2 = getelementptr inbounds i8*, i8** %argv, i32 2
  %3 = load i8*, i8** %2, align 1
  %4 = alloca [8 x i8]
  store [8 x i8] c"sendMsg\00", [8 x i8]* %4
  %5 = getelementptr inbounds [8 x i8], [8 x i8]* %4, i8 0, i8 0
  %6 = call i32 @strcmp(i8* %1, i8* %5)
  %7 = icmp eq i32 %6, 0
  br i1 %7, label %sendMsg, label %continue

sendMsg:                                          ; preds = %entry
  %8 = call i32* @json_string(i8* %3)
  %9 = call i32* @sendMsg(i32* %8)
  br label %continue

continue:                                         ; preds = %sendMsg, %entry
  %10 = alloca [11 x i8]
  store [11 x i8] c"executeMsg\00", [11 x i8]* %10
  %11 = getelementptr inbounds [11 x i8], [11 x i8]* %10, i8 0, i8 0
  %12 = call i32 @strcmp(i8* %1, i8* %11)
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %executeMsg, label %continue1

executeMsg:                                       ; preds = %continue
  %14 = call i32* @json_string(i8* %3)
  %15 = call i32* @executeMsg(i32* %14)
  br label %continue1

continue1:                                        ; preds = %executeMsg, %continue
  ret i32 0
}
